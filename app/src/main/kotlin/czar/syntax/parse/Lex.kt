package czar.syntax.parse

import czar.diag.Diag
import czar.diag.Report
import czar.syntax.S
import czar.syntax.Source
import czar.syntax.Span
import czar.syntax.hir.Ident
import czar.unreachable

private const val BUF_CAP = 3
private const val EOF = '\u0000'
private const val RAW_MARKER = '#'

private enum class IdentKind {
    NORMAL,
    RAW,
}

internal sealed class Mode(val start: Int) {
    var level: Int = 0;

    class Normal(start: Int): Mode(start)
    class StringLit(start: Int, val rawLen: Int?): Mode(start)
    class StringLitSubst(start: Int): Mode(start)
}

internal class Lex(val src: Source, val diag: Diag) {
    private var pos: Int = 0
    private val buf: MutableList<S<Token>> = mutableListOf()
    private val modeStack: MutableList<Mode> = mutableListOf()
    init {
        modeStack.add(Mode.Normal(0))
    }

    fun next(): S<Token> {
        val r = nth(0)
        if (buf.isNotEmpty()) {
            buf.removeAt(0)
        }
        return r
    }

    fun nth(i: Int): S<Token> {
        fillBuf()
        return buf[i]
    }

    fun ident(span: Span): Ident {
        val t = text(span)
        val raw = t.length >= 2 && t[1] == RAW_MARKER
        return Ident(if (raw) t.subSequence(2, t.length) else t)
    }

    fun stringLit(span: Span): Sequence<CharSequence> {
        val rawLen = if (src.text[span.start] == 'r') {
            var v = 0
            while (v < src.text.length && src.text[span.start + 1 + v] == RAW_MARKER) {
                v += 1
            }
            v
        } else {
            null
        }
        val payload = Span(span.start + if (rawLen == null) 1 else rawLen + 2, span.end)
        check(src.text[payload.start - 1] == '"')
        val t = text(payload)
        return if (rawLen == null) {
            unescape(t, payload.start)
        } else {
            sequenceOf(t)
        }
    }

    fun stringLitEnd(tok: S<Token>): Sequence<CharSequence> {
        require(tok.value == Token.STRING_LIT_END || tok.value == Token.RAW_STRING_LIT_END)
        val start: Int
        val rawLen = if (tok.value == Token.RAW_STRING_LIT_END) {
            var rawLen = 0
            while (src.text[tok.span.end - rawLen - 1] != '"') {
                rawLen += 1
            }
            start = if (tok.span.length > 1 &&
                    src.text[tok.span.start] == 'r' &&
                    src.text[tok.span.start + rawLen + 1] == '"' &&
                    (1..rawLen).all { src.text[tok.span.start + it] == RAW_MARKER }) {
                rawLen + 2
            } else {
                0
            }
            rawLen
        } else {
            start = if (tok.span.length > 1 && src.text[tok.span.start] == '"') {
                1
            } else {
                0
            }
            null
        }
        val payload = Span(tok.span.start + start, tok.span.end - (rawLen ?: 0) - 1)
        check(src.text[payload.end] == '"')
        val t = text(payload)
        return if (rawLen == null) {
            unescape(t, payload.start)
        } else {
            sequenceOf(t)
        }
    }

    fun stringLitSubstEnd(span: Span): CharSequence {
        val start = if (src.text[span.start] == ':') {
            1
        } else {
            check(span.length == 1)
            0
        }
        check(src.text[span.end - 1] == '}')
        return text(Span(span.start + start, span.end - 1))
    }

    fun docComment(span: Span): CharSequence {
        TODO()
    }

    private fun unescape(s: CharSequence, start: Int): Sequence<CharSequence> {
        return sequence {
            var i = 0
            while (i < s.length) {
                val chars = when (s[i]) {
                    '\r' -> {
                        i += 1
                        "\n"
                    }
                    '\\' -> {
                        val chars = when (s[i + 1]) {
                            'b' -> "\b"
                            'n' -> "\n"
                            'r' -> "\r"
                            't' -> "\t"
                            'u' -> {
                                val (unescaped, inc) = unicodeEscape(start + i + 2, s.subSequence(i + 2, s.length))
                                i += inc
                                unescaped
                            }
                            '\'' -> "'"
                            '"' -> "\""
                            '\\' -> "\\"
                            '{' -> "{"
                            else -> {
                                error(Span(start + i, start + i + 2), "invalid escape")
                                s.subSequence(i + 1, i + 2).toString()
                            }
                        }
                        i += 1
                        chars
                    }
                    else -> s.subSequence(i, i + 1)
                }
                i += 1
                if (chars.isNotEmpty()) {
                    yield(chars)
                }
            }
        }
    }

    private fun unicodeEscape(start: Int, s: CharSequence): Pair<CharSequence, Int> {
        if (s[0] != '{') {
            error(Span.one(start), "expected '{'")
            return Pair("", 0)
        }

        var endMarker = 1
        while (endMarker < s.length && isHexDigit(s[endMarker])) {
            endMarker += 1
        }
        if (endMarker == 1) {
            error(Span(start + 1, start + endMarker + 1), "expected unicode code point")
            return Pair("", 0)
        }
        if (endMarker == s.length) {
            error(Span(start, start + endMarker), "unterminated unicode escape")
            return Pair("", 0)
        }
        if (s[endMarker] != '}') {
            error(Span(start + endMarker, start + endMarker + 1), "expected '}'")
            return Pair("", 0)
        }

        val span = Span(start + 1, start + endMarker)

        val cpStr = s.subSequence(1, endMarker)
        val cp = cpStr.toString().toLongOrNull(16)
        if (endMarker - 1 > 6 || cp == null || cp < 0 || cp > 0x10ffff || cp in 0xd800..0xdfff) {
            error(span, "invalid unicode code point")
            return Pair(cpStr, endMarker + 1)
        }
        val res = String(Character.toChars(cp.toInt()))
        return Pair(res, endMarker + 1)
    }

    private fun mode(): Mode {
        return modeStack.last()
    }

    private fun text(span: Span): CharSequence {
        return src.text.subSequence(span.start, span.end)
    }

    private fun nthChar(i: Int): Char {
        return src.text.getOrNull(pos + i) ?: EOF
    }

    private fun nextChar(): Char {
        val r = nthChar(0)
        if (pos < src.text.length) {
            pos += 1
        }
        return r
    }

    private fun fillBuf() {
        while (buf.size < BUF_CAP) {
            val tok = scan()
            if (tok != null) {
                buf.add(tok)
            }
        }
    }

    private fun scan(): S<Token>? {
        if (mode() is Mode.StringLit) {
            return if (nthChar(0) == '{') {
                val tok = S(Span(pos, pos + 1), Token.STRING_LIT_SUBST_START)
                nextChar()
                modeStack.add(Mode.StringLitSubst(tok.span.start))
                tok
            } else {
                stringLitNext()
            }
        }

        val start = pos
        var identKind: IdentKind? = null
        var tok2: Token? = null
        val tok = when (val c = nextChar()) {
            EOF -> Token.EOF
            '{' -> {
                mode().level += 1
                Token.BRACE_OPEN
            }
            '}' -> {
                val mode = mode()
                when (mode) {
                    is Mode.Normal -> {
                        mode().level -= 1
                        Token.BRACE_CLOSE
                    }
                    is Mode.StringLit -> unreachable()
                    is Mode.StringLitSubst -> {
                        if (mode.level == 0) {
                            modeStack.removeLast()
                            check(mode() is Mode.StringLit)
                            Token.STRING_LIT_SUBST_END
                        } else {
                            Token.BRACE_CLOSE
                        }
                    }
                }
            }
            '[' -> {
                mode().level += 1
                Token.BRACKET_OPEN
            }
            ']' -> {
                mode().level -= 1
                Token.BRACKET_CLOSE
            }
            '(' -> {
                mode().level += 1
                Token.PAREN_OPEN
            }
            ')' -> {
                mode().level -= 1
                Token.PAREN_CLOSE
            }
            ':' -> {
                val mode = mode()
                when (mode) {
                    is Mode.Normal -> Token.COLON
                    is Mode.StringLit -> unreachable()
                    is Mode.StringLitSubst -> {
                        if (mode.level == 0) {
                            val end = src.text.indexOf('}', pos)
                            if (end == -1) {
                                fatal(Span(mode.start, src.text.length), "unterminated string subst")
                            }
                            pos = end + 1
                            modeStack.removeLast()
                            check(mode() is Mode.StringLit)
                            Token.STRING_LIT_SUBST_END
                        } else {
                            Token.COLON
                        }
                    }
                }
            }
            ',' -> Token.COMMA
            '.' -> Token.DOT
            '=' -> when (nthChar(0)) {
                '>' -> {
                    nextChar()
                    Token.EQ_GT
                }
                '=' -> {
                    nextChar()
                    Token.EQ_EQ
                }
                else -> Token.EQ
            }
            '<' -> if (maybeChar('=')) Token.LT_EQ else Token.LT
            '>' -> if (maybeChar('=')) Token.GT_EQ else Token.GT
            ';' -> Token.SEMI
            '|' -> Token.PIPE
            '\n' -> Token.NL
            '\r' -> {
                if (nextChar() != '\n') {
                    fatal(Span(start, pos), "invalid line ending")
                }
                Token.NL
            }
            ifChar(c) { docComment(c) } -> Token.DOC_COMMENT
            ifChar(c) { lineComment(c) } -> return null
            ifChar(c) { blockComment(c, start) } -> return null
            '/' -> Token.SLASH
            ifChar(c) {
                if (isWhitespace(c)) {
                    advanceWhile(::isWhitespace);
                    true
                } else {
                    false
                }
            } -> return null
            ifChar(c) { tok2 = stringLitStart(c, start); tok2 != null } -> tok2!!
            ifChar(c) { intLiteral(c) } -> Token.INT_LITERAL
            ifChar(c) {
                identKind = ident(c)
                identKind != null
            } -> keywordOrIdent(start, identKind!!)
            else -> {
                if (isEof()) {
                    Token.EOF
                } else {
                    fatal(Span(start, pos), "invalid character")
                }
            }
        }
        return S(Span(start, pos), tok)
    }

    private fun maybeChar(c: Char): Boolean {
        return if (nthChar(0) == c) {
            nextChar()
            true
        } else {
            false
        }
    }

    private fun keywordOrIdent(start: Int, identKind: IdentKind): Token {
        val ident = ident(Span(start, pos))
        if (identKind == IdentKind.RAW) {
            return Token.IDENT
        }
        return when (ident) {
            Ident("break") -> Token.KW_BREAK
            Ident("continue") -> Token.KW_CONTINUE
            Ident("false") -> Token.KW_FALSE
            Ident("fn") -> Token.KW_FN
            Ident("for") -> Token.KW_FOR
            Ident("in") -> Token.KW_IN
            Ident("impl") -> Token.KW_IMPL
            Ident("loop") -> Token.KW_LOOP
            Ident("mut") -> Token.KW_MUT
            Ident("pub") -> Token.KW_PUB
            Ident("self") -> Token.KW_SELF_LOWER
            Ident("Self") -> Token.KW_SELF_UPPER
            Ident("trait") -> Token.KW_TRAIT
            Ident("true") -> Token.KW_TRUE
            Ident("type") -> Token.KW_TYPE
            Ident("while") -> Token.KW_WHILE
            else -> Token.IDENT
        }
    }

    private fun ident(c: Char): IdentKind? {
        when (c) {
            in 'a'..'z', in 'A'..'Z' -> {}
            else -> return null
        }
        val kind = if (c == 'r' && nthChar(0) == RAW_MARKER) {
            nextChar()
            IdentKind.RAW
        } else {
            IdentKind.NORMAL
        }
        advanceWhile {
            when (it) {
                in 'a'..'z', in 'A'..'Z', in '0'..'9', '_' -> true
                else -> false
            }
        }
        return kind
    }

    private fun stringLitStart(c: Char, start: Int): Token? {
        val rawLen = if (c == 'r' && (nthChar(0) == '"' ||
                    nthChar(0) == RAW_MARKER && (nthChar(1) == RAW_MARKER || nthChar(1) == '"'))) {
            var v = 0
            advanceUntil {
                if (nthChar(0) != '"') {
                    v += 1
                    null
                } else {
                    1
                }
            }
            v
        } else if (c == '"') {
            null
        } else {
            return null
        }

        modeStack.add(Mode.StringLit(start, rawLen))

        return scanString()
    }

    private fun stringLitNext(): S<Token> {
        val start = pos
        val tok = scanString()
        return S(Span(start, pos), tok)
    }

    private fun scanString(): Token {
        val mode = mode() as Mode.StringLit
        val substs = mode.rawLen == null

        while (true) {
            if (substs && nthChar(0) == '{') {
                return Token.STRING_LIT
            }
            when (nextChar()) {
                EOF -> fatal(Span(mode.start, pos), "unterminated string")
                '"' -> {
                    val end = mode.rawLen == null || mode.rawLen == 0 ||
                        ((mode.rawLen - 1) downTo 0).all { nthChar(it) == RAW_MARKER }
                    if (end) {
                        pos += mode.rawLen ?: 0
                        modeStack.removeLast()
                        return if (mode.rawLen == null) Token.STRING_LIT_END else Token.RAW_STRING_LIT_END
                    }
                }
                '\\' -> if (mode.rawLen == null) {
                    // Skip \u{
                    if (nextChar() == 'u' && nthChar(0) == '{') {
                        nextChar()
                    }

                }
                '\r' -> when (nextChar()) {
                    EOF -> {} // report as unterminated
                    '\n' -> {}
                    else -> fatal(Span(pos - 1, pos), "invalid line ending");
                }
            }
        }
    }

    private fun intLiteral(c: Char): Boolean {
        when (c) {
            in '0'..'9', '-' -> {}
            else -> return false
        }
        advanceWhile { it in '0'..'9' }
        return true
    }

    private fun isEof(): Boolean {
        return pos == src.text.length
    }

    private fun advanceWhile(f: (Char) -> Boolean) {
        while (true) {
            if (isEof()) {
                break
            }
            if (!f(nthChar(0))) {
                break
            }
            nextChar()
        }
    }

    private fun advanceUntil(f: () -> Int?): Boolean {
        while (true) {
            if (isEof()) {
                return false
            }
            val i = f()
            if (i == null) {
                nextChar()
            } else {
                check(i >= 0 && pos + i <= src.text.length)
                pos += i
                return true
            }

        }
    }

    private fun ifChar(c: Char, expected: () -> Boolean): Char {
        return if (expected()) {
            c
        } else {
            EOF
        }
    }

    private fun lineComment(first: Char): Boolean {
        if (first != '/' || nthChar(0) != '/') {
            return false
        }

        nextChar()

        advanceUntil {
            if (nthChar(0) == '\n') 1 else null
        }

        return true
    }

    private fun blockComment(first: Char, start: Int): Boolean {
        if (first != '/' || nthChar(0) != '*') {
            return false
        }

        nextChar()

        var depth = 1
        while (true) {
            val c = nextChar()
            val c2 = nthChar(0)
            when {
                c == '/' && c2 == '*' -> {
                    nextChar()
                    depth += 1
                }
                c == '*' && c2 == '/' -> {
                    nextChar()
                    depth -= 1
                    if (depth == 0) {
                        break
                    }
                }
                c == EOF -> break
            }
        }

        if (depth != 0) {
            fatal(Span(start, pos), "unterminated comment");
        }

        return true
    }

    private fun docComment(first: Char): Boolean {
        return if (first == '/' && nthChar(0) == '/' && nthChar(1) == '/') {
            TODO()
        } else {
            false
        }
    }

    private fun fatal(span: Span, msg: String): Nothing {
        error(span, msg)
        throw ParseException()
    }

    private fun error(span: Span, msg: String) {
        diag.report(Report(src, span, msg))
    }
}

private fun isWhitespace(c: Char): Boolean {
    return when (c) {
        '\u0009', // \t
        '\u000B', // vertical tab
        '\u000C', // form feed
        '\u0020', // space
        -> true
        else -> false
    }
}

private fun isHexDigit(c: Char): Boolean {
     return c in '0'..'9' || c in 'a'..'f' || c in 'A'..'F'
}