package czar.syntax.parse

import czar.diag.Diag
import czar.syntax.S
import czar.syntax.Source
import czar.test.AutoExpect
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.DynamicTest.dynamicTest
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestFactory
import org.junit.jupiter.api.extension.ExtendWith
import java.nio.file.Path

@ExtendWith(AutoExpect::class)
class LexerTest {
    @TestFactory
    internal fun parseOk() = listOf(
        "" to "",
        "\u0009\u000B\u000C\u0020" to "",
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
        "//\n\r\n" to "{NL} {NL}",
        "/*\n\r\n*/" to "",
        "foo // bar  \n  foo ( /* bar (/* !!! */) )\r\n*/)" to "r#foo {NL} r#foo ( )",
        "'a'' ''\t''\b''\\u{10fffF}'" to "C{61} C{20} C{9} C{8} C{10ffff}",
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
        "'",
        "'foobar",
        "'''abcd''\n''foo\nbar\r\n'",
        "r#_ r#self r#Self",
        ).mapIndexed { i, inp ->
            dynamicTest("$i") {
                parseFail(i.toString(), inp)
            }
        }

    @Test
    internal fun tokens() {
        for (tok in Token.values()) {
            val inp = when (tok) {
                Token.CHAR_LIT -> "'a'"
                Token.EOF -> ""
                Token.IDENT -> "main"
                Token.NL -> "\n"
                Token.RAW_STRING_LIT_END,
                Token.STRING_LIT,
                Token.STRING_LIT_END,
                Token.STRING_LIT_SUBST_END,
                Token.STRING_LIT_SUBST_START,
                 -> null
                else -> tok.toString()
            }
            if (inp != null) {
                val diag = Diag()
                val lex = Lexer(Source(inp, Path.of("test")), diag)
                assertEquals(tok, lex.next().value)
                assertEquals(Token.EOF, lex.next().value)
                assertTrue(diag.reports.isEmpty()) { diag.toString() }
            }
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

            Token.STRING_LIT -> "S{${lexer.stringLit(tok.span).joinToString("")}}"
            Token.STRING_LIT_END, Token.RAW_STRING_LIT_END -> "E{${lexer.stringLitEnd(tok).joinToString("")}}"
            Token.STRING_LIT_SUBST_START -> "{SS}"
            Token.STRING_LIT_SUBST_END -> "SE{${lexer.stringLitSubstEnd(tok.span)}}"
            Token.NL -> "{NL}"
            Token.CHAR_LIT -> "C{${lexer.charLit(tok.span).toString(16)}}"

            Token.BRACE_CLOSE,
            Token.BRACE_OPEN,
            Token.BRACKET_CLOSE,
            Token.BRACKET_OPEN,
            Token.COLON,
            Token.COMMA,
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
            Token.KW_AS,
            Token.KW_AS_BANG,
            Token.KW_AS_QUEST,
            Token.KW_AS_PERCENT,
            Token.KW_CONST,
            Token.KW_ELSE,
            Token.KW_ENUM,
            Token.KW_IF,
            Token.KW_IS,
            Token.KW_MATCH,
            Token.KW_MODULE,
            Token.KW_NOT,
            Token.KW_PACKAGE,
            Token.KW_RET,
            Token.KW_STATIC,
            Token.KW_STRUCT,
            Token.KW_SUPER,
            Token.KW_UNDERSCORE,
            Token.KW_UNSAFE,
            Token.KW_USE,
            Token.KW_WHERE,
            Token.AMP,
            Token.AMP2,
            Token.AMP_EQ,
            Token.BANG,
            Token.BANG_EQ,
            Token.COLON2,
            Token.DASH,
            Token.DASH_EQ,
            Token.DASH_GT,
            Token.DOT2_EQ,
            Token.DOT2,
            Token.DOT3,
            Token.GT2_EQ,
            Token.GT2,
            Token.HAT,
            Token.HAT_EQ,
            Token.LT2_EQ,
            Token.LT2,
            Token.PERCENT,
            Token.PERCENT_EQ,
            Token.PIPE2,
            Token.PIPE_EQ,
            Token.PLUS,
            Token.PLUS_EQ,
            Token.QUEST,
            Token.SLASH_EQ,
            Token.STAR,
            Token.STAR_EQ,
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
        assertFalse(diag.reports.isEmpty())
        AutoExpect.verify(id, diag.toString())
    }
}