package czar.syntax.parse

import czar.diag.Diag
import czar.syntax.Source
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.DynamicTest.dynamicTest
import org.junit.jupiter.api.TestFactory
import java.nio.file.Path

class LexTest {
    @TestFactory
    internal fun parseOk() = listOf(
        "" to "",
        "{}[]():,.=<==== < => >;/|\n\r\n\u0009\u000B\u000C\u0020i dent r#break break" to
                "{ } [ ] ( ) : , . = <= == = < => > ; / | <NL> <NL> r#i r#dent r#break break",
        """ "" "_" "\t\b\n\r\'\"\\" """ to "S{} S{_} S{\t\b\n\r'\"\\}",
        """ "\u{0}\u{000000}\u{00d7ff}\u{e000}\u{10fffF}" """ to "S{\u0000\u0000\ud7ff\ue000\udbff\udfff}",
        """ r"" r#"\t\b\n\r\'\"\\"#  """ to """S{} S{\t\b\n\r\'\"\\}""",
        "r##########\"\\t\\b\\n\\r\\'\\\"\\\\\r\n\"#########\"##########" to "S{\\t\\b\\n\\r\\'\\\"\\\\\r\n\"#########}"
        ).mapIndexed { i, (inp, exp) ->
            dynamicTest("$i") {
                parseOk(inp, exp)
            }
        }

    fun parseOk(inp: String, exp: String) {
        val diag = Diag()
        val lex = Lex(Source(inp, Path.of("test")), diag)
        var first = true
        val act = StringBuilder()
        while (true) {
            val tok = try {
                lex.next()
            } catch (_: ParseException) {
                break
            }
            val s = when (tok.value) {
                Token.EOF -> break

                Token.IDENT -> "r#${lex.ident(tok.span)}"
                Token.INT_LITERAL -> TODO()
                Token.STRING_LITERAL -> "S{${lex.stringLiteral(tok.span).joinToString("")}}"

                Token.BRACE_CLOSE,
                Token.BRACE_OPEN,
                Token.BRACKET_CLOSE,
                Token.BRACKET_OPEN,
                Token.COLON,
                Token.COMMA,
                Token.DOC_COMMENT,
                Token.DOT,
                Token.EQ,
                Token.EQ_EQ,
                Token.EQ_GT,
                Token.GT,
                Token.GT_EQ,
                Token.KW_BREAK,
                Token.KW_CONTINUE,
                Token.KW_FALSE,
                Token.KW_FN,
                Token.KW_FOR,
                Token.KW_IMPL,
                Token.KW_IN,
                Token.KW_LOOP,
                Token.KW_MUT,
                Token.KW_PUB,
                Token.KW_SELF_LOWER,
                Token.KW_SELF_UPPER,
                Token.KW_TRAIT,
                Token.KW_TRUE,
                Token.KW_TYPE,
                Token.KW_WHILE,
                Token.LT,
                Token.LT_EQ,
                Token.NL,
                Token.PAREN_CLOSE,
                Token.PAREN_OPEN,
                Token.PIPE,
                Token.SEMI,
                Token.SLASH,
                -> tok.value.toString()
            }
            if (first) {
                first = false
            } else {
                act.append(' ')
            }
            act.append(s)
        }
        assert(diag.reports.isEmpty()) { diag.toString() }
        assertEquals(exp, act.toString())
    }
}