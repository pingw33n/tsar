package czar.syntax.parse

import czar.diag.Diag
import czar.syntax.S
import czar.syntax.Source
import czar.test.AutoExpect
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.DynamicTest.dynamicTest
import org.junit.jupiter.api.TestFactory
import org.junit.jupiter.api.extension.ExtendWith
import java.nio.file.Path

@ExtendWith(AutoExpect::class)
class LexerTest {
    @TestFactory
    internal fun parseOk() = listOf(
        "" to "",
        "{}[]():,.=<==== < => >;/|\n\r\n\u0009\u000B\u000C\u0020i dent r#break break" to
                "{ } [ ] ( ) : , . = <= == = < => > ; / | {NL} {NL} r#i r#dent r#break break",
        """ "" "_" "\t\b\n\r\'\"\\" """ to "E{} E{_} E{\t\b\n\r'\"\\}",
        """ "\u{0}\u{000000}\u{00d7ff}\u{e000}\u{10fffF}" """ to "E{\u0000\u0000\ud7ff\ue000\udbff\udfff}",
        """ r"" r#"\t\b\n\r\'\"\\"#  """ to """E{} E{\t\b\n\r\'\"\\}""",
        "r##########\"\\t\\b\\n\\r\\'\\\"\\\\\r\n\"#########\"##########" to "E{\\t\\b\\n\\r\\'\\\"\\\\\r\n\"#########}",
        """ "{}" """ to "S{} {SS} SE{} E{}",
        """ "a\r\n{}" """ to "S{a\r\n} {SS} SE{} E{}",
        """ "a\r\n{}\tb\b" """ to "S{a\r\n} {SS} SE{} E{\tb\b}",
        """ "a\r\n{:}\tb\b" """ to "S{a\r\n} {SS} SE{} E{\tb\b}",
        """ "a\r\n{: "format"\n\r :x-:}\tb\b" """ to "S{a\r\n} {SS} SE{ \"format\"\\n\\r :x-:} E{\tb\b}",
        """ "a{"b{("c": d)${'\n'}:${"\t"}inner${"\r\n"}}d":outer}e" """ to
            "S{a} {SS} S{b} {SS} ( E{c} : r#d ) {NL} SE{\tinner\r\n} E{d} SE{outer} E{e}",
        """ r"{}" r###"##{###}#"### """ to "E{{}} E{##{###}#}",
        ).mapIndexed { i, (inp, exp) ->
            dynamicTest("$i") {
                parseOk(inp, exp)
            }
        }

    @TestFactory
    internal fun parseFail() = listOf(
        "\r",
        "foo\n\r",
        "\uD83E\uDD70",
        "\"",
        "\n\"str\ning",
        "\"\r",
        "foo/*",
        "foo/*\nbar",
        "foo/*/*\nbar*/",
        """ "{:" """,
        """ "\a\ \z\u\u{\u{ag}\u{d800}\u{dfff}\u{110000}\u{fffffffffffffffffffff}" """,
        ).mapIndexed { i, inp ->
            dynamicTest("$i") {
                parseFail(i.toString(), inp)
            }
        }

    private fun parseOk(inp: String, exp: String) {
        val diag = Diag()
        val lex = Lexer(Source(inp, Path.of("test")), diag)
        var first = true
        val act = StringBuilder()
        while (true) {
            val tok = try {
                lex.next()
            } catch (_: ParseException) {
                break
            }
            val s = toString(lex, tok) ?: break
            if (first) {
                first = false
            } else {
                act.append(' ')
            }
            act.append(s)
        }
        assertTrue(diag.reports.isEmpty()) { diag.toString() }
        assertEquals(exp, act.toString())
    }

    private fun toString(lexer: Lexer, tok: S<Token>): String? {
        return when (tok.value) {
            Token.EOF -> null

            Token.IDENT -> "r#${lexer.ident(tok.span)}"
            Token.INT_LITERAL -> TODO()

            Token.STRING_LIT -> "S{${lexer.stringLit(tok.span).joinToString("")}}"
            Token.STRING_LIT_END, Token.RAW_STRING_LIT_END -> "E{${lexer.stringLitEnd(tok).joinToString("")}}"
            Token.STRING_LIT_SUBST_START -> "{SS}"
            Token.STRING_LIT_SUBST_END -> "SE{${lexer.stringLitSubstEnd(tok.span)}}"
            Token.NL -> "{NL}"

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
            Token.PAREN_CLOSE,
            Token.PAREN_OPEN,
            Token.PIPE,
            Token.SEMI,
            Token.SLASH,
            -> tok.value.toString()
        }
    }

    private fun parseFail(id: String, inp: String) {
        val diag = Diag()
        val lex = Lexer(Source(inp, Path.of("test")), diag)
        try {
            while (true) {
                val tok = lex.next()
                if (tok.value == Token.EOF) {
                    break
                }
                toString(lex, tok)
            }
        } catch (_: ParseException) {
        }
        AutoExpect.verify(id, diag.toString())
    }
}