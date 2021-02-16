package czar.syntax.parse

enum class Token {
    BRACE_CLOSE,
    BRACE_OPEN,
    BRACKET_CLOSE,
    BRACKET_OPEN,
    COLON,
    COMMA,
    DOC_COMMENT,
    DOT,
    EOF,
    EQ,
    EQ_EQ,
    EQ_GT,
    GT,
    GT_EQ,
    IDENT,
    INT_LITERAL,
    KW_BREAK,
    KW_CONTINUE,
    KW_FALSE,
    KW_FN,
    KW_FOR,
    KW_IMPL,
    KW_IN,
    KW_LOOP,
    KW_MUT,
    KW_PUB,
    KW_SELF_LOWER,
    KW_SELF_UPPER,
    KW_TRAIT,
    KW_TRUE,
    KW_TYPE,
    KW_WHILE,
    LT,
    LT_EQ,
    NL,
    PAREN_CLOSE,
    PAREN_OPEN,
    PIPE,
    SEMI,
    SLASH,
    STRING_LITERAL,
    ;

    override fun toString(): String {
        return when (this) {
            BRACE_CLOSE -> "}"
            BRACE_OPEN -> "{"
            BRACKET_CLOSE -> "]"
            BRACKET_OPEN -> "["
            COLON -> ":"
            COMMA -> ","
            DOC_COMMENT -> "///"
            DOT -> "."
            EOF -> "<EOF>"
            EQ -> "="
            EQ_EQ -> "=="
            EQ_GT -> "=>"
            GT -> ">"
            GT_EQ -> ">="
            IDENT -> "<IDENT>"
            INT_LITERAL -> "<INT_LITERAL>"
            KW_BREAK -> "break"
            KW_CONTINUE -> "continue"
            KW_FALSE -> "false"
            KW_FN -> "fn"
            KW_FOR -> "for"
            KW_IMPL -> "impl"
            KW_IN -> "in"
            KW_LOOP -> "loop"
            KW_MUT -> "mut"
            KW_PUB -> "pub"
            KW_SELF_LOWER -> "self"
            KW_SELF_UPPER -> "Self"
            KW_TRAIT -> "trait"
            KW_TRUE -> "true"
            KW_TYPE -> "type"
            KW_WHILE -> "while"
            LT -> "<"
            LT_EQ -> "<="
            NL -> "<NL>"
            PAREN_CLOSE -> ")"
            PAREN_OPEN -> "("
            PIPE -> "|"
            SEMI -> ";"
            SLASH -> "/"
            STRING_LITERAL -> "<STRING_LITERAL>"
        }
    }}

class ParseException: RuntimeException()

//private class Parser(val src: Source, val diag: Diag) {
//    val lex = Lex(src, diag)
//
//    fun parse(): Module {
//        return module()
//    }
//
//    private fun module(): Module {

//    }
//
//    private fun maybeDocComment(): S<CharSequence>? {
//        return maybe(Token.DOC_COMMENT)?.let { S(it.span, lex.docComment(it.span)) }
//    }
//
//    private fun maybe(tok: Token): S<Token>? {
//        return if (lex.nth(0).value == tok) {
//            lex.next()
//        } else {
//            null
//        }
//    }
//
//    private fun token(expected: Token): S<Token> {
//        val actual = lex.next()
//        if (actual.value != expected) {
//            error(actual.span, "expected $expected, found ${actual.value}")
//            throw ParseException()
//        }
//        return actual
//    }
//
//    private fun ident(): S<Ident> {
//        val span = token(Token.IDENT).span
//        return S(span, lex.ident(span))
//    }
//
//    private fun maybeIdent(expected: Ident): S<Unit>? {
//        val tok = lex.nth(0)
//        return if (tok.value == Token.IDENT && lex.ident(tok.span) == expected) {
//            lex.next()
//            S(tok.span, Unit)
//        } else {
//            null
//        }
//    }
//
//    private fun intLiteral(): S<Long> {
//        val span = token(Token.INT_LITERAL).span
//        return S(span, lex.long(span))
//    }
//
//    private fun stringLiteral(): S<CharSequence> {
//        val span = token(Token.STRING_LITERAL).span
//        return S(span, lex.string(span))
//    }
//
//    private fun maybeLiteral(): S<Literal>? {
//        val tok = lex.nth(0)
//        val v = when (tok.value) {
//            Token.INT_LITERAL -> {
//                lex.next()
//                Literal.Int(lex.long(tok.span))
//            }
//            Token.KW_FALSE -> {
//                lex.next()
//                Literal.Bool(false)
//            }
//            Token.KW_TRUE -> {
//                lex.next()
//                Literal.Bool(true)
//            }
//            Token.STRING_LITERAL -> {
//                lex.next()
//                Literal.String(lex.string(tok.span))
//            }
//            Token.IDENT -> {
//                val path = path(false)
//
//                val args = if (maybe(Token.PAREN_OPEN) != null) {
//                    val args = mutableListOf<S<Literal>>()
//                    while (true) {
//                        val lit = maybeLiteral() ?: break
//                        args.add(lit)
//                        maybe(Token.COMMA) ?: break
//                    }
//                    token(Token.PAREN_CLOSE)
//                    args
//                } else {
//                    emptyList()
//                }
//
//                Literal.Data(path, args)
//            }
//            else -> return null
//        }
//        return tok.map { v }
//    }
//
//    private fun literal(): S<Literal> {
//        return maybeLiteral() ?: fatal(lex.nth(0).span, "expected literal, found ${lex.nth(0).value}")
//    }
//
//    private fun error(span: Span, msg: String) {
//        diag.report(Report(src, span, msg))
//    }
//
//    private fun fatal(span: Span, msg: String): Nothing {
//        error(span, msg)
//        throw ParseException()
//    }
//}
//
//fun parse(src: Source, diag: Diag): Module? {
//    val errsBefore = diag.reports.size
//    return try {
//        Parser(src, moduleResolver, diag).parse()
//    } catch (e: ParseException) {
//        check(diag.reports.size > errsBefore) { e.stackTraceToString() }
//        null
//    }
//}