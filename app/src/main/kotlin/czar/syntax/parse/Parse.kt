package czar.syntax.parse

import czar.Checkpointed
import czar.diag.Diag
import czar.diag.Report
import czar.setOnce
import czar.syntax.S
import czar.syntax.Source
import czar.syntax.Span
import czar.syntax.hir.*

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
    DASH_PERCENT,
    DASH_PERCENT_EQ,
    DASH_GT,
    DOT,
    DOT2_EQ,
    DOT2,
    DOT3,
    EOF,
    EQ,
    EQ2,
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
    PLUS_PERCENT,
    PLUS_PERCENT_EQ,
    QUEST, // ?
    RAW_STRING_LIT_END,
    SEMI,
    SLASH,
    SLASH_EQ,
    STAR,
    STAR_EQ,
    STAR_PERCENT,
    STAR_PERCENT_EQ,
    STRING_LIT,
    STRING_LIT_END,
    STRING_LIT_SUBST_END,
    STRING_LIT_SUBST_START,
    ;

    fun isKeyword(): Boolean = when (this) {
        KW_AS,
        KW_AS_BANG,
        KW_AS_PERCENT,
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
        -> true
        AMP,
        AMP2,
        AMP_EQ,
        BANG,
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
        DASH,
        DASH_EQ,
        DASH_PERCENT,
        DASH_PERCENT_EQ,
        DASH_GT,
        DOT,
        DOT2_EQ,
        DOT2,
        DOT3,
        EOF,
        EQ,
        EQ2,
        EQ_GT,
        FLOAT_LIT,
        GT,
        GT2_EQ,
        GT2,
        GT_EQ,
        HAT,
        HAT_EQ,
        IDENT,
        INT_LIT,
        LABEL,
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
        PLUS_PERCENT,
        PLUS_PERCENT_EQ,
        QUEST,
        RAW_STRING_LIT_END,
        SEMI,
        SLASH,
        SLASH_EQ,
        STAR,
        STAR_EQ,
        STAR_PERCENT,
        STAR_PERCENT_EQ,
        STRING_LIT,
        STRING_LIT_END,
        STRING_LIT_SUBST_END,
        STRING_LIT_SUBST_START,
        -> false
    }

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
            DASH_PERCENT -> "-%"
            DASH_PERCENT_EQ -> "-%="
            DOT -> "."
            DOT2 -> ".."
            DOT2_EQ -> "..="
            DOT3 -> "..."
            EQ -> "="
            EQ2 -> "=="
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
            PLUS_PERCENT -> "+%"
            PLUS_PERCENT_EQ -> "+%="
            QUEST -> "?"
            SEMI -> ";"
            SLASH -> "/"
            SLASH_EQ -> "/="
            STAR -> "*"
            STAR_EQ -> "*="
            STAR_PERCENT -> "*%"
            STAR_PERCENT_EQ -> "*%="

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

internal class ParseException: RuntimeException()

private data class ExprCtx(
    val prec: PrecGroup = PrecGroup.LOWEST,
    val opChain: MutableList<S<OpKind>> = mutableListOf(),
) {
    fun operand(prec: PrecGroup): ExprCtx {
        return copy(prec = prec)
    }
}

enum class OpAssoc {
    LEFT_TO_RIGHT,
    RIGHT_TO_LEFT,
}

enum class PrecGroup(
    val prec: Int,
    val assoc: OpAssoc = OpAssoc.LEFT_TO_RIGHT,
): Comparable<PrecGroup> {
    SELECTOR(180),
    FN_CALL(170),
    UNARY_POSTFIX(160),
    UNARY_PREFIX(150, OpAssoc.RIGHT_TO_LEFT),
    AS(140),
    MUL(130),
    ADD(120),
    BIT_SHIFT(110),
    BIT_AND(100),
    BIT_XOR(90),
    BIT_OR(80),
    CMP(70),
    AND(60),
    OR(50),
    RANGE(40),
    ASSIGN(30, OpAssoc.RIGHT_TO_LEFT),
    LOWEST(0),
    ;

    fun isLowerThan(other: PrecGroup): Boolean {
        return when (other.assoc) {
            OpAssoc.LEFT_TO_RIGHT -> prec <= other.prec
            OpAssoc.RIGHT_TO_LEFT -> prec < other.prec
        }
    }
}

private sealed class OpKind {
    abstract fun prec(): PrecGroup

    abstract override fun toString(): String

    object As: OpKind() {
        override fun prec() = PrecGroup.AS
        override fun toString() = "`as` expression"
    }

    data class Binary(val value: Expr.BinaryOp.Kind): OpKind() {
        override fun prec(): PrecGroup {
            return when (value) {
                Expr.BinaryOp.Kind.ADD_ASSIGN,
                Expr.BinaryOp.Kind.ASSIGN,
                Expr.BinaryOp.Kind.BIT_AND_ASSIGN,
                Expr.BinaryOp.Kind.BIT_OR_ASSIGN,
                Expr.BinaryOp.Kind.BIT_XOR_ASSIGN,
                Expr.BinaryOp.Kind.DIV_ASSIGN,
                Expr.BinaryOp.Kind.MUL_ASSIGN,
                Expr.BinaryOp.Kind.OF_ADD_ASSIGN,
                Expr.BinaryOp.Kind.OF_MUL_ASSIGN,
                Expr.BinaryOp.Kind.OF_SUB_ASSIGN,
                Expr.BinaryOp.Kind.REM_ASSIGN,
                Expr.BinaryOp.Kind.SHL_ASSIGN,
                Expr.BinaryOp.Kind.SHR_ASSIGN,
                Expr.BinaryOp.Kind.SUB_ASSIGN,
                -> PrecGroup.ASSIGN

                Expr.BinaryOp.Kind.ADD,
                Expr.BinaryOp.Kind.OF_ADD,
                Expr.BinaryOp.Kind.OF_SUB,
                Expr.BinaryOp.Kind.SUB,
                -> PrecGroup.ADD

                Expr.BinaryOp.Kind.AND -> PrecGroup.AND
                Expr.BinaryOp.Kind.OR -> PrecGroup.OR
                Expr.BinaryOp.Kind.BIT_AND -> PrecGroup.BIT_AND
                Expr.BinaryOp.Kind.BIT_OR -> PrecGroup.BIT_OR
                Expr.BinaryOp.Kind.BIT_XOR -> PrecGroup.BIT_XOR

                Expr.BinaryOp.Kind.DIV,
                Expr.BinaryOp.Kind.MUL,
                Expr.BinaryOp.Kind.OF_MUL,
                Expr.BinaryOp.Kind.REM,
                -> PrecGroup.MUL

                Expr.BinaryOp.Kind.EQ,
                Expr.BinaryOp.Kind.GT,
                Expr.BinaryOp.Kind.GT_EQ,
                Expr.BinaryOp.Kind.LT,
                Expr.BinaryOp.Kind.LT_EQ,
                Expr.BinaryOp.Kind.NOT_EQ,
                -> PrecGroup.CMP

                Expr.BinaryOp.Kind.RANGE_EXCL,
                Expr.BinaryOp.Kind.RANGE_INCL,
                -> PrecGroup.RANGE

                Expr.BinaryOp.Kind.SHL,
                Expr.BinaryOp.Kind.SHR,
                -> PrecGroup.BIT_SHIFT
            }
        }

        override fun toString() = "`$value`"
    }

    object FnCall: OpKind() {
        override fun prec() = PrecGroup.FN_CALL
        override fun toString() = "function call"
    }

    object Index: OpKind() {
        override fun prec() = PrecGroup.FN_CALL
        override fun toString() = "indexing expression"
    }

    object Range: OpKind() {
        override fun prec() = PrecGroup.RANGE
        override fun toString() = "range expression"
    }

    object Selector: OpKind() {
        override fun prec() = PrecGroup.SELECTOR
        override fun toString() = "selector expression"
    }

    data class Unary(val value: Expr.UnaryOp.Kind): OpKind() {
        override fun prec(): PrecGroup {
            return when (value) {
                Expr.UnaryOp.Kind.ADDR_OF,
                Expr.UnaryOp.Kind.DEREF,
                Expr.UnaryOp.Kind.NEG,
                Expr.UnaryOp.Kind.NOT,
                -> PrecGroup.UNARY_PREFIX

                Expr.UnaryOp.Kind.PANICKING_UNWRAP,
                Expr.UnaryOp.Kind.PROPAGATING_UNWRAP,
                -> PrecGroup.UNARY_POSTFIX
            }
        }

        override fun toString() = "`$value`"
    }
}

private enum class PathPos {
    EXPR,
    IMPORT,
    TYPE,
}

private class Parser(val src: Source, val diag: Diag) {
    private class State(
        private val diag: Diag,
        val spans: MutableMap<Node.Id<*>, Span> = mutableMapOf(),
    ): Checkpointed.State<State> {
        private val initialErrorCount: Int = diag.reports.size

        val newErrorCount: Int
            get() = diag.reports.size - initialErrorCount

        override fun copy(): State {
            return State(diag, spans.toMutableMap())
        }
    }

    private val checkpointed: Checkpointed<State> = Checkpointed(State(diag))
    private val lex: Lexer = Lexer(src, diag)

    private val spans
        get() = checkpointed.state.spans

    fun parse(): Hir {
        val items = moduleItems()
        val module = spanned(Span(0, src.text.length), Module(src, name = null, items))
        // TODO check all Nodes have spans
        return Hir(module, spans)
    }

    private fun checkpoint() {
        checkpointed.checkpoint()
        lex.checkpoint()
        diag.checkpoint()

    }

    private fun rollback() {
        checkpointed.rollback()
        lex.rollback()
        diag.rollback()
    }

    private fun commit() {
        checkpointed.commit()
        lex.commit()
        diag.commit()
    }

    private fun moduleItems(): List<ModuleItem> {
        val items = mutableListOf<ModuleItem>()
        while (!isPrefix(Token.EOF)) {
            val item = moduleItem()
            if (item == null) {
                unexpected(lex.at(0), "module item")
            }
            items.add(item)
        }
        return items
    }

    private fun isPrefix(vararg toks: Token): Boolean {
        for (i in toks.indices) {
            if (lex.at(i).value != toks[i]) {
                return false
            }
        }
        return true
    }

    private fun moduleItem(): ModuleItem? {
        return when {
            isPrefix(Token.KW_FN) ||
            isPrefix(Token.KW_UNSAFE, Token.KW_FN) ||
            isPrefix(Token.KW_PUB, Token.KW_FN) ||
            isPrefix(Token.KW_PUB, Token.KW_UNSAFE, Token.KW_FN)
            -> fnDef()

            else -> null
        }
    }

    private fun maybePub(): S<Pub>? {
        val span = maybe(Token.KW_PUB)?.span ?: return null
        val scope = if (maybe(Token.PAREN_OPEN) != null) {
            val scopeSpan = expect(Token.KW_PACKAGE).span
            expect(Token.PAREN_CLOSE)
            S(scopeSpan, Pub.Scope.PACKAGE)
        } else {
            null
        }
        return S(span, Pub(scope))
    }

    private fun fnDef(): FnDef {
        val span = spanner()

        val pub = maybePub()
        val unsafe = maybe(Token.KW_UNSAFE)?.map { }

        check(lex.next().value == Token.KW_FN)

        val name = ident()
        val typeParams = typeParams()
        val params = mutableListOf<FnParam>()
        commaDelimited(Token.PAREN_OPEN, Token.PAREN_CLOSE) {
            val paramSpan = spanner()
            val node = if (params.isEmpty() &&
                    (isPrefix(Token.KW_SELF_LOWER) || isPrefix(Token.AMP, Token.KW_SELF_LOWER))) {
                val ref = maybe(Token.AMP)
                val paramName = lex.next().map {
                    check(it == Token.KW_SELF_LOWER)
                    Ident.SELF_LOWER
                }

                val label = paramName.map { Ident.SELF_LOWER }

                val selfSpan = paramName.span
                val selfPath = spanned(selfSpan, Path.of(S(selfSpan, Ident.SELF_UPPER)))
                val selfType = spanned(selfSpan, TypeExpr.Path(selfPath))
                val type = if (ref != null) {
                    val n = spanned(paramSpan(), TypeExpr.Ref(selfType))
                    n
                } else {
                    selfType
                }

                FnParam(label, paramName, type)
            } else {
                val label = fnParamLabel()
                val paramName = ident()
                expect(Token.COLON)
                val type = typeExpr()
                FnParam(label, paramName, type)
            }
            params.add(spanned(paramSpan(), node))
        }
        val result = if (maybe(Token.DASH_GT) != null) {
            typeExpr()
        } else {
            null
        }

        // FIXME this is for initial impl only
        val hasBody = !name.value.value.startsWith("__extern__")

        val body = if (hasBody) {
            block()
        } else {
            null
        }

        return spanned(span(), FnDef(pub, name, typeParams, params, result, unsafe, body, variadic = false))
    }

    private fun fnParamLabel(): S<Ident>? {
        if (lex.at(1).value != Token.IDENT) {
            return null
        }
        val tok = lex.at(0)
        return when {
            tok.value == Token.IDENT -> {
                ident()
            }
            tok.value.isKeyword() -> {
                lex.next().map { Ident(it.toString()) }
            }
            else -> {
                null
            }
        }
    }

    private fun block(): Block {
        val span = spanner()
        expect(Token.BRACE_OPEN)
        val items = mutableListOf<Block.Item>()
        lex.withNlMode(Lexer.NlMode.ALLOW) {
            while (true) {
                if (maybe(Token.NL) != null) {
                    continue
                }
                if (maybe(Token.BRACE_CLOSE) != null) {
                    break
                }

                val moduleItem = moduleItem()
                if (moduleItem != null) {
                    items.add(Block.Item.ModuleItem(moduleItem))
                    continue
                }

                val expr = maybeExpr()
                if (expr != null) {
                    items.add(Block.Item.Expr(expr))
                    continue
                }

                unexpected(lex.at(0), "module item or expression")
            }
        }
        return spanned(span(), Block(items))
    }

    private fun typeExpr(): TypeExpr {
        val span = spanner()
        val path = maybePath(PathPos.TYPE)
        val node = if (path == null) {
            when {
                maybe(Token.AMP) != null -> TypeExpr.Ref(typeExpr())
                maybe(Token.BRACKET_OPEN) != null -> {
                    val item = typeExpr()
                    val len = if (maybe(Token.SEMI) != null) {
                        maybeExpr()
                    } else {
                        null
                    }
                    expect(Token.BRACKET_CLOSE)
                    TypeExpr.Slice(item, len)
                }
                isPrefix(Token.PAREN_OPEN) -> {
                    val fields = mutableListOf<StructBody.Field>()
                    val record = isPrefix(Token.PAREN_OPEN, Token.IDENT, Token.COLON)
                    val hadTrailingComma = commaDelimited(Token.PAREN_OPEN, Token.PAREN_CLOSE) {
                        val name = if (record) {
                            val name = ident()
                            expect(Token.COLON)
                            name
                        } else {
                            null
                        }
                        val type = typeExpr()
                        val name_ = name ?: S(spans[type.id]!!, Ident.index(fields.size))
                        fields.add(StructBody.Field(pub = null, name_, type))
                    }!!
                    if (fields.size != 1 || record || hadTrailingComma) {
                        TypeExpr.UnnamedStruct(StructBody(fields))
                    } else {
                        TODO()
                    }
                }
                else -> unexpected(lex.at(0), "`&`, `[`, `(` or path")
            }
        } else {
            TypeExpr.Path(path)
        }
        return spanned(span(), node)
    }

    private fun <T: Node>spanned(span: Span, node: T): T {
        spans.setOnce(node.id, span)
        return node
    }

    private fun intLit(): Expr.Int {
        val tok = lex.next()
        check(tok.value == Token.INT_LIT)
        val span = tok.span
        return spanned(span, Expr.Int(lex.intLit(span)))
    }

    private fun floatLit(): Expr.Float? {
        val span = when (lex.at(0).value) {
            Token.INT_LIT -> {
                val int = lex.at(0)
                val dot = lex.at(1)
                val frac = lex.at(2)
                if (dot.value == Token.DOT &&
                    frac.value == Token.INT_LIT &&
                    dot.span.start == int.span.end &&
                    frac.span.start == dot.span.end) {

                    lex.next()
                    lex.next()
                    lex.next()
                    Span(int.span.start, frac.span.end)
                } else {
                    return null
                }
            }
            Token.FLOAT_LIT -> lex.next().span
            else -> return null
        }
        return spanned(span, Expr.Float(lex.floatLit(span)))
    }

    private fun maybeExpr(ctx: ExprCtx = ExprCtx()): Expr? {
        var span = spanner()

        val leftOpKind = when (lex.at(0).value) {
            Token.DASH -> OpKind.Unary(Expr.UnaryOp.Kind.NEG)
            Token.STAR -> OpKind.Unary(Expr.UnaryOp.Kind.DEREF)
            Token.AMP, Token.AMP2 -> OpKind.Unary(Expr.UnaryOp.Kind.ADDR_OF)
            Token.KW_NOT -> OpKind.Unary(Expr.UnaryOp.Kind.NOT)
            else -> null
        }
        if (leftOpKind != null) {
            ctx.opChain.add(S(lex.at(0).span, leftOpKind))
        }
        var left = when (leftOpKind) {
            is OpKind.Unary -> unaryOp(span, leftOpKind.value, arg = null, ctx)
            else -> null
        } ?: when (lex.at(0).value) {
            Token.FLOAT_LIT -> floatLit()!!
            Token.INT_LIT -> floatLit() ?: intLit()
            Token.PAREN_OPEN -> lex.withNlMode(Lexer.NlMode.SKIP) {
                lex.next()
                val firstField = unnamedStructLiteralField()
                val firstValue = if (firstField == null) {
                        maybeExpr()
                    } else {
                        expr()
                    }
                if (firstValue == null) {
                    expect(Token.PAREN_CLOSE)
                    spanned(span(), Expr.FnCall(callee = null, emptyList()))
                } else if (firstField != null || lex.at(0).value == Token.COMMA) {
                    val args = mutableListOf(Expr.FnCall.Arg(firstField, firstValue))
                    while (true) {
                        val comma = maybe(Token.COMMA)
                        if (maybe(Token.PAREN_CLOSE) != null) {
                            break
                        }
                        if (comma == null) {
                            unexpected(lex.at(0), Token.COMMA, Token.PAREN_CLOSE)
                        }

                        val field = unnamedStructLiteralField()
                        val value = expr()
                        args.add(Expr.FnCall.Arg(field, value))
                    }
                    spanned(span(), Expr.FnCall(callee = null, args))
                } else {
                    expect(Token.PAREN_CLOSE)
                    firstValue
                }
            }
            else -> {
                val path = maybePath(PathPos.EXPR) ?: return null
                spanned(span(), Expr.Path(path))
            }
        }

        while (true) {
            if (isPrefix(Token.NL, Token.DOT)) {
                lex.next()
            }

            val opKind = when (lex.at(0).value) {
                Token.AMP -> Expr.BinaryOp.Kind.BIT_AND
                Token.AMP2 -> Expr.BinaryOp.Kind.AND
                Token.AMP_EQ -> Expr.BinaryOp.Kind.BIT_AND_ASSIGN
                Token.BANG_EQ -> Expr.BinaryOp.Kind.NOT_EQ
                Token.DASH -> Expr.BinaryOp.Kind.SUB
                Token.DASH_EQ -> Expr.BinaryOp.Kind.SUB_ASSIGN
                Token.DASH_PERCENT -> Expr.BinaryOp.Kind.OF_SUB
                Token.DASH_PERCENT_EQ -> Expr.BinaryOp.Kind.OF_SUB_ASSIGN
                Token.DOT2 -> Expr.BinaryOp.Kind.RANGE_EXCL
                Token.DOT2_EQ -> Expr.BinaryOp.Kind.RANGE_INCL
                Token.EQ -> Expr.BinaryOp.Kind.ASSIGN
                Token.EQ2 -> Expr.BinaryOp.Kind.EQ
                Token.GT -> Expr.BinaryOp.Kind.GT
                Token.GT2 -> Expr.BinaryOp.Kind.SHR
                Token.GT2_EQ -> Expr.BinaryOp.Kind.SHR_ASSIGN
                Token.GT_EQ -> Expr.BinaryOp.Kind.GT_EQ
                Token.HAT -> Expr.BinaryOp.Kind.BIT_XOR
                Token.HAT_EQ -> Expr.BinaryOp.Kind.BIT_XOR_ASSIGN
                Token.LT -> Expr.BinaryOp.Kind.LT
                Token.LT2 -> Expr.BinaryOp.Kind.SHL
                Token.LT2_EQ -> Expr.BinaryOp.Kind.SHL_ASSIGN
                Token.LT_EQ -> Expr.BinaryOp.Kind.LT_EQ
                Token.PERCENT -> Expr.BinaryOp.Kind.REM
                Token.PERCENT_EQ -> Expr.BinaryOp.Kind.REM_ASSIGN
                Token.PIPE -> Expr.BinaryOp.Kind.BIT_OR
                Token.PIPE2 -> Expr.BinaryOp.Kind.OR
                Token.PIPE_EQ -> Expr.BinaryOp.Kind.BIT_OR_ASSIGN
                Token.PLUS -> Expr.BinaryOp.Kind.ADD
                Token.PLUS_EQ -> Expr.BinaryOp.Kind.ADD_ASSIGN
                Token.PLUS_PERCENT -> Expr.BinaryOp.Kind.OF_ADD
                Token.PLUS_PERCENT_EQ -> Expr.BinaryOp.Kind.OF_ADD_ASSIGN
                Token.SLASH -> Expr.BinaryOp.Kind.DIV
                Token.SLASH_EQ -> Expr.BinaryOp.Kind.DIV_ASSIGN
                Token.STAR -> Expr.BinaryOp.Kind.MUL
                Token.STAR_EQ -> Expr.BinaryOp.Kind.MUL_ASSIGN
                Token.STAR_PERCENT -> Expr.BinaryOp.Kind.OF_MUL
                Token.STAR_PERCENT_EQ -> Expr.BinaryOp.Kind.OF_MUL_ASSIGN
                else -> null
            }?.let { OpKind.Binary(it) } ?: when (lex.at(0).value) {
                Token.BANG -> Expr.UnaryOp.Kind.PANICKING_UNWRAP
                Token.QUEST -> Expr.UnaryOp.Kind.PROPAGATING_UNWRAP
                else -> null
            }?.let { OpKind.Unary(it) } ?: when (lex.at(0).value) {
                Token.KW_AS,
                Token.KW_AS_PERCENT,
                Token.KW_AS_BANG,
                -> OpKind.As

                Token.DOT -> OpKind.Selector
                Token.DOT2, Token.DOT2_EQ -> OpKind.Range

                Token.BRACKET_OPEN -> OpKind.Index

                else -> {
                    checkOpChain(ctx.opChain)
                    break
                }
            }

            val nextPrec = opKind.prec()

            if (nextPrec.isLowerThan(ctx.prec)) {
                break
            }

            ctx.opChain.add(S(lex.at(0).span, opKind))

            val nextCtx = ctx.operand(nextPrec)

            left = when (opKind) {
                OpKind.As -> {
                    lex.next()
                    val type = typeExpr()
                    spanned(span(), Expr.As(left, type))
                }
                is OpKind.Binary -> binaryOp(span, opKind.value, left, nextCtx)
                OpKind.Range -> TODO()
                OpKind.Selector -> {
                    lex.next()
                    val name = if (lex.at(0).value == Token.INT_LIT) {
                        val sp = lex.next().span
                        S(sp, lex.ident(sp))
                    } else {
                        ident()
                    }
                    spanned(span(), Expr.Selector(left, name))
                }
                is OpKind.Unary -> unaryOp(span, opKind.value, left, nextCtx)
                OpKind.FnCall -> TODO()
                OpKind.Index -> {
                    lex.next().also { check(it.value == Token.BRACKET_OPEN) }
                    val index = lex.withNlMode(Lexer.NlMode.SKIP) { expr() }
                    expect(Token.BRACKET_CLOSE)
                    spanned(span(), Expr.Index(left, index))
                }
            }

            span = spanner()
        }

        return left
    }

    private fun unnamedStructLiteralField(): S<Ident>? {
        if (lex.at(1).value != Token.COLON) {
            return null
        }
        val r = when (lex.at(0).value) {
            Token.INT_LIT -> {
                val sp = lex.next().span
                S(sp, lex.ident(sp))
            }
            Token.IDENT -> ident()
            else -> return null
        }
        lex.next()
        return r
    }

    private fun checkOpChain(opChain: MutableList<S<OpKind>>) {
        while (opChain.size >= 2) {
            val a = opChain[0].value
            val b = opChain[1].value
            when {
                a.prec() == PrecGroup.CMP && b.prec() == PrecGroup.CMP ->
                    error(opChain[1].span, "comparison operators can't be directly chained")
                a is OpKind.As -> if (!canFollowAsOp(b)) {
                    error(opChain[1].span, "${OpKind.As} can't be directly followed by $b")
                }
            }

            opChain.removeAt(0)
        }
    }

    private fun canFollowAsOp(op: OpKind): Boolean {
        return when (op) {
            is OpKind.As,
            is OpKind.Range,
            -> true

            is OpKind.Selector,
            OpKind.FnCall,
            -> false

            is OpKind.Binary -> when (op.value) {
                Expr.BinaryOp.Kind.ADD,
                Expr.BinaryOp.Kind.ADD_ASSIGN,
                Expr.BinaryOp.Kind.AND,
                Expr.BinaryOp.Kind.ASSIGN,
                Expr.BinaryOp.Kind.BIT_AND,
                Expr.BinaryOp.Kind.BIT_AND_ASSIGN,
                Expr.BinaryOp.Kind.BIT_OR,
                Expr.BinaryOp.Kind.BIT_OR_ASSIGN,
                Expr.BinaryOp.Kind.BIT_XOR,
                Expr.BinaryOp.Kind.BIT_XOR_ASSIGN,
                Expr.BinaryOp.Kind.DIV,
                Expr.BinaryOp.Kind.DIV_ASSIGN,
                Expr.BinaryOp.Kind.EQ,
                Expr.BinaryOp.Kind.GT,
                Expr.BinaryOp.Kind.GT_EQ,
                Expr.BinaryOp.Kind.LT,
                Expr.BinaryOp.Kind.LT_EQ,
                Expr.BinaryOp.Kind.MUL,
                Expr.BinaryOp.Kind.MUL_ASSIGN,
                Expr.BinaryOp.Kind.NOT_EQ,
                Expr.BinaryOp.Kind.OF_ADD,
                Expr.BinaryOp.Kind.OF_ADD_ASSIGN,
                Expr.BinaryOp.Kind.OF_MUL,
                Expr.BinaryOp.Kind.OF_MUL_ASSIGN,
                Expr.BinaryOp.Kind.OF_SUB,
                Expr.BinaryOp.Kind.OF_SUB_ASSIGN,
                Expr.BinaryOp.Kind.OR,
                Expr.BinaryOp.Kind.RANGE_EXCL,
                Expr.BinaryOp.Kind.RANGE_INCL,
                Expr.BinaryOp.Kind.REM,
                Expr.BinaryOp.Kind.REM_ASSIGN,
                Expr.BinaryOp.Kind.SHL,
                Expr.BinaryOp.Kind.SHL_ASSIGN,
                Expr.BinaryOp.Kind.SHR,
                Expr.BinaryOp.Kind.SHR_ASSIGN,
                Expr.BinaryOp.Kind.SUB,
                Expr.BinaryOp.Kind.SUB_ASSIGN,
                -> true
            }

            is OpKind.Unary -> when (op.value) {
                Expr.UnaryOp.Kind.ADDR_OF,
                Expr.UnaryOp.Kind.DEREF,
                Expr.UnaryOp.Kind.NEG,
                Expr.UnaryOp.Kind.NOT,
                -> throw IllegalArgumentException()

                Expr.UnaryOp.Kind.PANICKING_UNWRAP,
                Expr.UnaryOp.Kind.PROPAGATING_UNWRAP,
                -> false
            }

            is OpKind.Index -> false
        }
    }

    private fun binaryOp(
        span: () -> Span,
        kind: Expr.BinaryOp.Kind,
        left: Expr,
        ctx: ExprCtx,
    ): Expr {
        val kindSpan = lex.next().span
        maybe(Token.NL)
        val right = expr(ctx)
        return spanned(span(), Expr.BinaryOp(S(kindSpan, kind), left, right))
    }

    private fun expr(ctx: ExprCtx = ExprCtx()): Expr {
        return maybeExpr(ctx) ?: unexpected(lex.at(0), "expression")
    }

    private fun unaryOp(span: () -> Span, kind: Expr.UnaryOp.Kind, arg: Expr?, ctx: ExprCtx): Expr.UnaryOp {
        val kindSpan = (maybeSplit(Token.AMP) ?: lex.next()).span
        val argu = when (kind) {
            Expr.UnaryOp.Kind.ADDR_OF,
            Expr.UnaryOp.Kind.DEREF,
            Expr.UnaryOp.Kind.NEG,
            Expr.UnaryOp.Kind.NOT,
            -> {
                check(arg == null)
                maybe(Token.NL)
                expr(ctx.operand(PrecGroup.UNARY_PREFIX))
            }

            Expr.UnaryOp.Kind.PANICKING_UNWRAP,
            Expr.UnaryOp.Kind.PROPAGATING_UNWRAP,
            -> arg!!
        }
        return spanned(span(), Expr.UnaryOp(S(kindSpan, kind), argu))
    }

    private fun maybePath(pos: PathPos): Path? {
        val span = spanner()
        val origin: S<Path.Origin>? = when (lex.at(0).value) {
            Token.IDENT -> null
            Token.COLON2 -> {
                lex.next()
                Path.Origin.Package(null)
            }
            Token.KW_PACKAGE -> {
                lex.next()

                val name = if (maybe(Token.PAREN_OPEN) != null) {
                    val r = ident()
                    expect(Token.PAREN_CLOSE)
                    r.value
                } else {
                    null
                }

                expect(Token.COLON2)

                Path.Origin.Package(name)
            }
            Token.KW_SUPER -> {
                lex.next()
                var count = 1
                while (true) {
                    expect(Token.COLON2)
                    if (maybe(Token.KW_SUPER) == null) {
                        break
                    }
                    count += 1
                }
                Path.Origin.Super(count)
            }
            else -> return null
        }?.let { S(span(), it) }
        val prefix = mutableListOf<S<Path.Item>>()
        val suffix = mutableListOf<S<Path.SuffixItem>>()
        while (true) {
            val suffixItem = maybePathSuffixItem(pos)
            if (suffixItem == null) {
                if (maybe(Token.BRACE_OPEN) != null) {
                    commaDelimited(Token.BRACE_OPEN, Token.BRACE_CLOSE) {
                        suffix.add(pathSuffixItem(pos))
                    }
                } else {
                    unexpected(lex.at(0))
                }
            } else {
                if (suffixItem.value is Path.SuffixItem.Item && suffixItem.value.renamedAs == null) {
                    if (maybe(Token.COLON2) != null) {
                        if (suffixItem.value.item.value.name.value == Ident.SELF_LOWER) {
                            error(suffixItem.value.item.value.name.span,
                                "`self` imports are only allowed within a { } list")
                        }
                        prefix.add(suffixItem.value.item)
                        continue
                    } else {
                        suffix.add(suffixItem)
                    }
                }
            }
            break
        }

        return spanned(span(), Path(origin, prefix, suffix))
    }

    private fun spanner(): () -> Span {
        val start = lex.at(0).span.start
        return { Span(start, lex.at(-1).span.end) }
    }

    private fun pathRenamedAs(): S<Ident>? {
        return if (maybe(Token.KW_AS) != null) {
            ident()
        } else {
            null
        }
    }

    private fun maybePathSuffixItem(pos: PathPos): S<Path.SuffixItem>? {
        val span = spanner()
        val item = when (lex.at(0).value) {
            Token.IDENT -> {
                val itemSpan = spanner()
                val ident = ident()
                val typeArgs = typeArgs(pos == PathPos.EXPR)
                val renamedAs = if (pos == PathPos.IMPORT) pathRenamedAs() else null
                Path.SuffixItem.Item(S(itemSpan(), Path.Item(ident, typeArgs)), renamedAs)
            }
            Token.STAR -> {
                if (pos == PathPos.IMPORT) {
                    lex.next()
                    Path.SuffixItem.Star
                } else {
                    return null
                }
            }
            Token.KW_SELF_LOWER -> {
                if (pos == PathPos.IMPORT) {
                    val ident = lex.next().map { Ident.SELF_LOWER }
                    val renamedAs = pathRenamedAs()
                    Path.SuffixItem.Item(ident.map { Path.Item(ident, emptyList()) }, renamedAs)
                } else {
                    return null
                }
            }
            else -> return null
        }
        return S(span(), item)
    }

    private fun pathSuffixItem(pos: PathPos): S<Path.SuffixItem> {
        return maybePathSuffixItem(pos) ?: unexpected(lex.at(0), Token.STAR, Token.KW_SELF_LOWER)
    }

    private fun typeArgs(inExprPos: Boolean): List<TypeExpr> {
        when (lex.at(0).value) {
            Token.LT, Token.LT2 -> {}
            else -> return emptyList()
        }
        if (inExprPos) {
            checkpoint()
        }
        try {
            val r = mutableListOf<TypeExpr>()
            lex.withNlMode(Lexer.NlMode.SKIP) {
                commaDelimited(Token.LT, Token.GT) {
                    r.add(typeExpr())
                }
            }
            val ok = !inExprPos || checkpointed.state.newErrorCount == 0 &&
                when (lex.at(0).value) {
                    Token.BANG,
                    Token.BRACE_CLOSE,
                    Token.BRACE_OPEN,
                    Token.BRACKET_CLOSE,
                    Token.BRACKET_OPEN,
                    Token.COMMA,
                    Token.COLON2,
                    Token.DOT,
                    Token.EOF,
                    Token.NL,
                    Token.PAREN_CLOSE,
                    Token.PAREN_OPEN,
                    Token.QUEST,
                    Token.SEMI,
                    -> true
                    else -> false
                }
            if (ok) {
                if (inExprPos) {
                    commit()
                }
                return r
            }
        } catch (e: ParseException) {
            if (!inExprPos) {
                throw e
            }
        }
        if (inExprPos) {
            rollback()
        }
        return emptyList()
    }

    private fun commaDelimited(start: Token, end: Token, f: () -> Unit): Boolean? {
        maybe(start) ?: return null
        if (maybeSplit(end) != null) {
            return false
        }
        while (true) {
            f()

            val comma = maybe(Token.COMMA) != null
            if (maybeSplit(end) != null) {
                return comma
            }
            if (!comma) {
                unexpected(lex.at(0), Token.COMMA)
            }
        }
    }

    private fun typeParams(): List<S<Ident>> {
        val r = mutableListOf<S<Ident>>()
        commaDelimited(Token.LT, Token.GT) {
            r.add(ident())
        }
        return r
    }

    private fun maybeSplit(tok: Token): S<Token>? {
        val unsplit = when (tok) {
            Token.GT -> Token.GT2
            Token.AMP -> Token.AMP2
            else -> return maybe(tok)
        }
        val next = lex.at(0)
        when (next.value) {
            tok -> {
                lex.next()
                return next
            }
            unsplit -> {}
            else -> return null
        }

        lex.next()
        check(next.span.length == 2)
        lex.push(S(Span.one(next.span.start + 1), tok))
        return S(Span.one(next.span.start), tok)
    }

    private fun maybeIdent(): S<Ident>? {
        val span = maybe(Token.IDENT)?.span ?: return null
        val v = lex.ident(span)
        return S(span, v)
    }

    private fun ident(): S<Ident> {
        return maybeIdent() ?: unexpected(lex.at(0), Token.IDENT)
    }

    private fun maybe(tok: Token): S<Token>? {
        return if (lex.at(0).value == tok) {
            lex.next()
        } else {
            null
        }
    }

    private fun expect(expected: Token): S<Token> {
        val actual = lex.next()
        if (actual.value != expected) {
            unexpected(actual, expected)
        }
        return actual
    }

    private fun displayToken(tok: Token): String {
        return when (tok) {
            Token.CHAR_LIT,
            Token.EOF,
            Token.FLOAT_LIT,
            Token.IDENT,
            Token.INT_LIT,
            Token.LABEL,
            Token.NL,
            Token.STRING_LIT,
            Token.STRING_LIT_END,
            Token.RAW_STRING_LIT_END,
            Token.STRING_LIT_SUBST_START,
            Token.STRING_LIT_SUBST_END,
            -> tok.toString()
            else -> "`$tok`"
        }
    }

    private fun unexpected(actual: S<Token>, vararg expected: Token): Nothing {
        unexpected(actual, expected.asSequence().map { displayToken(it) }.joinToString() )
    }

    private fun unexpected(actual: S<Token>, expected: CharSequence? = null): Nothing {
        val msg = if (expected == null) {
            "unexpected ${displayToken(actual.value)}"
        } else {
            "expected $expected, found ${displayToken(actual.value)}"
        }
        fatal(actual.span, msg)
    }

    private fun error(span: Span, msg: String) {
        diag.report(Report(src, span, msg))
    }

    private fun fatal(span: Span, msg: String): Nothing {
        error(span, msg)
        throw ParseException()
    }
}

fun parse(src: Source, diag: Diag): Hir? {
    val errsBefore = diag.reports.size
    return try {
        Parser(src, diag).parse()
    } catch (e: ParseException) {
        check(diag.reports.size > errsBefore) { e.stackTraceToString() }
        null
    }
}

fun main() {
    val diag = Diag()
    val hir = parse(
        Source("""
        fn foo<T, U, V>(break v: package(std)::T<F<X<Z>>>) {}
    """, java.nio.file.Path.of("test")), diag
    )
    if (diag.reports.isNotEmpty()) {
        println(diag.toString())
    }
    hir!!
}