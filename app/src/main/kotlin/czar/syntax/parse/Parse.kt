package czar.syntax.parse

enum class Token {
    AMP,
    AMP2,
    AMP_EQ,
    BANG, // !
    BANG_EQ,
    BRACE_CLOSE,
    BRACE_OPEN,
    BRACKET_CLOSE,
    BRACKET_EQ,
    BRACKET_OPEN,
    CHAR_LIT,
    COLON,
    COLON2,
    COMMA,
    DASH, // -
    DASH_EQ,
    DASH_GT,
    DOT,
    DOT2_EQ,
    DOT2,
    DOT3,
    EOF,
    EQ,
    EQ_EQ,
    EQ_GT,
    FLOAT_LIT,
    GT,
    GT2_EQ,
    GT2,
    GT_EQ,
    HAT, // ^
    HAT_EQ,
    IDENT,
    INT_LIT,
    KW_AS,
    KW_AS_BANG, // as!
    KW_AS_PERCENT, // as%
    KW_AS_QUEST, // as?
    KW_BREAK,
    KW_CONST,
    KW_CONTINUE,
    KW_ELSE,
    KW_ENUM,
    KW_FALSE,
    KW_FN,
    KW_FOR,
    KW_IF,
    KW_IMPL,
    KW_IN,
    KW_IS,
    KW_LOOP,
    KW_MATCH,
    KW_MODULE,
    KW_MUT,
    KW_NOT,
    KW_PACKAGE,
    KW_PUB,
    KW_RET,
    KW_SELF_LOWER,
    KW_SELF_UPPER,
    KW_STATIC,
    KW_STRUCT,
    KW_SUPER,
    KW_TRAIT,
    KW_TRUE,
    KW_TYPE,
    KW_UNDERSCORE,
    KW_UNSAFE,
    KW_USE,
    KW_WHERE,
    KW_WHILE,
    LABEL, // #ident
    LT,
    LT2_EQ,
    LT2,
    LT_EQ,
    NL,
    PAREN_CLOSE,
    PAREN_OPEN,
    PERCENT,
    PERCENT_EQ,
    PIPE,
    PIPE2,
    PIPE_EQ,
    PLUS,
    PLUS_EQ,
    QUEST, // ?
    RAW_STRING_LIT_END,
    SEMI,
    SLASH,
    SLASH_EQ,
    STAR,
    STAR_EQ,
    STRING_LIT,
    STRING_LIT_END,
    STRING_LIT_SUBST_END,
    STRING_LIT_SUBST_START,
    ;

    override fun toString(): String {
        return when (this) {
            AMP -> "&"
            AMP2 -> "&&"
            AMP_EQ -> "&="
            BANG -> "!"
            BANG_EQ -> "!="
            BRACE_CLOSE -> "}"
            BRACE_OPEN -> "{"
            BRACKET_CLOSE -> "]"
            BRACKET_EQ -> "[="
            BRACKET_OPEN -> "["
            COLON -> ":"
            COLON2 -> "::"
            COMMA -> ","
            DASH -> "-"
            DASH_EQ -> "-="
            DASH_GT -> "->"
            DOT -> "."
            DOT2 -> ".."
            DOT2_EQ -> "..="
            DOT3 -> "..."
            EQ -> "="
            EQ_EQ -> "=="
            EQ_GT -> "=>"
            GT -> ">"
            GT2 -> ">>"
            GT2_EQ -> ">>="
            GT_EQ -> ">="
            HAT -> "^"
            HAT_EQ -> "^="
            KW_AS -> "as"
            KW_AS_BANG -> "as!"
            KW_AS_PERCENT -> "as%"
            KW_AS_QUEST -> "as?"
            KW_BREAK -> "break"
            KW_CONST -> "const"
            KW_CONTINUE -> "continue"
            KW_ELSE -> "else"
            KW_ENUM -> "enum"
            KW_FALSE -> "false"
            KW_FN -> "fn"
            KW_FOR -> "for"
            KW_IF -> "if"
            KW_IMPL -> "impl"
            KW_IN -> "in"
            KW_IS -> "is"
            KW_LOOP -> "loop"
            KW_MATCH -> "match"
            KW_MODULE -> "module"
            KW_MUT -> "mut"
            KW_NOT -> "not"
            KW_PACKAGE -> "package"
            KW_PUB -> "pub"
            KW_RET -> "ret"
            KW_SELF_LOWER -> "self"
            KW_SELF_UPPER -> "Self"
            KW_STATIC -> "static"
            KW_STRUCT -> "struct"
            KW_SUPER -> "super"
            KW_TRAIT -> "trait"
            KW_TRUE -> "true"
            KW_TYPE -> "type"
            KW_UNDERSCORE -> "_"
            KW_UNSAFE -> "unsafe"
            KW_USE -> "use"
            KW_WHERE -> "where"
            KW_WHILE -> "while"
            LT -> "<"
            LT2 -> "<<"
            LT2_EQ -> "<<="
            LT_EQ -> "<="
            PAREN_CLOSE -> ")"
            PAREN_OPEN -> "("
            PERCENT -> "%"
            PERCENT_EQ -> "%="
            PIPE -> "|"
            PIPE2 -> "||"
            PIPE_EQ -> "|="
            PLUS -> "+"
            PLUS_EQ -> "+="
            QUEST -> "?"
            SEMI -> ";"
            SLASH -> "/"
            SLASH_EQ -> "/="
            STAR -> "*"
            STAR_EQ -> "*="

            CHAR_LIT,
            EOF,
            FLOAT_LIT,
            IDENT,
            INT_LIT,
            LABEL,
            NL,
            STRING_LIT,
            STRING_LIT_END,
            RAW_STRING_LIT_END,
            STRING_LIT_SUBST_START,
            STRING_LIT_SUBST_END,
            -> "<${this.name}>"
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