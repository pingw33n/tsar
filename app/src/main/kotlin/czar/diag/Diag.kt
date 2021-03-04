package czar.diag

import czar.Checkpointed
import czar.syntax.Source
import czar.syntax.Span
import java.io.StringWriter
import java.nio.file.Path
import kotlin.math.max

data class Report(
    val source: Source,
    val span: Span,
    val message: String,
)

private class State(val reports: MutableList<Report> = mutableListOf()): Checkpointed.State<State> {
    override fun copy(): State = State(reports.toMutableList())
}

class Diag(private val baseDir: Path? = null) {
    private val checkpointed: Checkpointed<State> = Checkpointed(State())

    val reports: List<Report>
        get() = checkpointed.state.reports

    fun checkpoint() = checkpointed.checkpoint()
    fun rollback() = checkpointed.rollback()
    fun commit() = checkpointed.commit()

    fun report(report: Report) {
        checkpointed.state.reports.add(report)
    }

    fun print(out: Appendable) {
        val absBaseDir = baseDir?.toAbsolutePath()
        val reps = reports.sortedWith(compareBy({ it.source.location.toString() }, { it.span.start }, { it.span.end }) )
        reps.forEachIndexed { i, rep ->
            if (i > 0) {
                out.append("\n")
            }
            val severity = "error"
            out.append("$severity: ${rep.message}\n")
            val path = absBaseDir?.relativize(rep.source.location.toAbsolutePath()) ?: rep.source.location
            if (!rep.span.isEmpty()) {
                val hi_line = HiLine.fromSpan(rep.span, rep.source);
                out.append("  --> $path:${hi_line.num}:${hi_line.col_start + 1}\n")
                hi_line.print(out, rep.source)
            } else {
                out.append("  --> $path\n")
            }
        }
    }

    override fun toString(): String {
        val w = StringWriter()
        print(w)
        return w.toString()
    }
}

private data class HiLine(
    val whole: Span,
    val num: Int,
    val col_start: Int,
    val col_len: Int,
) {
    companion object {
        fun fromSpan(span: Span, source: Source): HiLine {
            val s = source.text;
            require(!span.isEmpty());
            require(span.end <= s.length);

            var num = 1;
            var line_start = 0;
            var r: HiLine? = null;
            var i = 0
            while (i < s.length) {
                if (r == null) {
                    if (i == span.start) {
                        val col_start = i - line_start;
                        r = HiLine(
                            Span(line_start, line_start),
                            num,
                            col_start,
                            0)
                    } else {
                        require(i < span.start);
                    }
                }

                if (i == span.end) {
                    if (r != null) {
                        val col_len = (i - line_start)- r.col_start;
                        r = r.copy(col_len = col_len)
                    }
                }

                val c = s[i]
                val nl = when (c) {
                    '\n' -> 1
                    '\r' -> if (s.getOrNull(i) == '\n') {
                        i += 1
                        2
                    } else {
                        null
                    }
                    else -> null
                }
                val eof = i == s.length - 1
                if (nl != null || eof) {
                    if (r != null) {
                        val whole_end = i + if (eof) 1 else 0
                        val col_len = if (r.col_len == 0) {
                            (i - line_start) - r.col_start + if (eof) 1 else 0
                        } else {
                            r.col_len
                        }
                        r = r.copy(whole = r.whole.copy(end = whole_end), col_len = col_len)
                        break
                    }
                }
                if (nl != null) {
                    num += 1;
                    line_start = i + nl;
                }
                i += 1
            }
            return r!!
        }
    }


    fun print(out: Appendable, source: Source) {
        val line_num_width = digit_count(num);
        val line_num = num.toString();
        out.append(" ".repeat(line_num.length - line_num_width))
        out.append("$line_num | ${source.text.subSequence(whole.start, whole.end)}\n")

        val hi_start = line_num_width + 3 + col_start;
        out.append(" ".repeat(hi_start))
        out.append("~".repeat(max(col_len, 1)))
        out.append("\n")
    }
}

fun digit_count(n: Int): Int {
    var nn = n
    var r = 1;
    while (nn >= 10) {
        nn /= 10;
        r += 1;
    }
    return r
}