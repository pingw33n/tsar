package czar.syntax.hir

import czar.syntax.S
import czar.syntax.Source
import czar.syntax.Span
import czar.syntax.parse.Token
import java.math.BigDecimal
import java.math.BigInteger

inline class Ident(val value: CharSequence) {
    init {
        require(value.isNotEmpty())
    }

    companion object {
        val SELF_LOWER: Ident = Ident(Token.KW_SELF_LOWER.toString())
        val SELF_UPPER: Ident = Ident(Token.KW_SELF_UPPER.toString())
        val UNDERSCORE: Ident = Ident(Token.KW_UNDERSCORE.toString())
    }

    override fun toString(): String {
        return value.toString()
    }
}

val <T: Node> T.id get() = Node.Id(this)

sealed class Node {
    class Id<T: Node>(private val node_: T) {
        val node: T get() = node_

        override fun equals(other: Any?): Boolean {
            if (this === other) return true
            if (javaClass != other?.javaClass) return false

            other as Id<*>

            return node_ === other.node
        }

        override fun hashCode(): Int {
            return System.identityHashCode(node_)
        }

        override fun toString(): String {
            return "Node.Id@${hashCode().toString(16)}"
        }
    }
}

data class Module(
    val source: Source?,
    val name: S<Name>?,
    val pub: S<Pub>?,
    val items: List<Item>,
): Node() {
    data class Name(val name: S<Ident>, val pub: S<Pub>?)

    sealed class Item {
        data class FnDef(val value: czar.syntax.hir.FnDef): Item()
        data class Impl(val value: czar.syntax.hir.Impl): Item()
        data class Module(val value: czar.syntax.hir.Module): Item()
        data class StructDef(val value: czar.syntax.hir.StructDef): Item()
        data class TypeAlias(val value: czar.syntax.hir.TypeAlias): Item()
        data class Use(val value: czar.syntax.hir.Use): Item()
    }
}

data class Pub(val scope: S<Scope>?) {
    enum class Scope {
        PACKAGE,
    }
}

sealed class ModuleItem: Node()

data class FnDef(
    val pub: S<Pub>?,
    val name: S<Ident>,
    val typeParams: List<TypeParam>,
    val params: List<FnParam>,
    val result: TypeExpr?,
    val unsafe: S<Unit>?,
    val body: Block?,
    // C interop only
    val variadic: Boolean,
): ModuleItem()

data class FnParam(
    val labelFn: S<Label>?,
    val name: S<Ident>,
    val type: TypeExpr,
): ModuleItem() {
    inline class Label(val value: CharSequence) {
        init {
            require(value.isNotEmpty())
        }

        companion object {
            val SELF: Label = Label("self")
            val UNDERSCORE: Label = Label("_")
        }

        override fun toString(): String {
            return value.toString()
        }
    }
}

data class LetDef(
    val pattern: S<Pattern>,
    val type: TypeExpr?,
    val init: Expr?,
): Node()

data class TypeParam(val name: S<Ident>): Node()

sealed class TypeExpr: Node() {
    // &T
    data class Ref(val value: TypeExpr): TypeExpr()

    // [<item>]
    // [<item>; <len>]
    data class Slice(
        val item: TypeExpr,
        val len: Expr?,
    ): TypeExpr()

    // path::to::Type
    data class Path(val value: czar.syntax.hir.Path): TypeExpr()

    // (T,)
    // (foo: T, bar: U)
    data class UnnamedStruct(val value: StructBody): TypeExpr()
}

data class Block(val items: List<Item>): Node() {
    sealed class Item {
        data class Expr(val value: czar.syntax.hir.Expr): Item()
        data class ModuleItem(val value: Module.Item): Item()
    }
}

data class Path(
    val origin: S<Origin>?,

    // foo::bar<T, U>::baz::qux
    // ^^^^^^^^^^^^^^^^^^^
    val prefix: List<S<Item>>,

    // foo::bar<T, U>::baz::qux
    //                      ^^^
    // foo::bar<T, U>::baz::{self, *, qux}
    //                      ^^^^^^^^^^^^^^
    val suffix: List<S<SuffixItem>>
): Node() {
    sealed class Origin {
        // ::foo
        // package::foo
        // package(bar)::foo
        data class Package(val name: Ident?): Origin()

        // super::super::foo
        data class Super(val count: Int): Origin()
    }

    data class Item(
        val name: S<Ident>,
        val typeArgs: List<TypeExpr>,
    )

    sealed class SuffixItem {
        data class Item(
            val item: S<Path.Item>,
            val renamedAs: S<Ident>?,
        ): SuffixItem()

        object Star: SuffixItem()
    }
}

data class StructDef(
    val pub: S<Pub>?,
    val name: S<Ident>,
    val type_params: List<TypeParam>,
    val body: StructBody,
): ModuleItem()

data class StructBody(val fields: List<Field>): Node() {
    data class Field(
        val pub: S<Pub>?,
        val name: S<Ident>?,
        val type: TypeExpr,
    )
}

typealias Pattern = Ident

sealed class Expr: Node() {
    data class Bool(val value: Boolean): Expr()
    data class Char(val value: kotlin.Int): Expr()
    data class String(val value: CharSequence): Expr()
    data class Int(val value: BigInteger): Expr()
    data class Float(val value: BigDecimal): Expr()

    data class BinaryOp(
        val kind: S<Kind>,
        val left: Expr,
        val right: Expr,
    ): Expr() {
        enum class Kind {
            ADD,
            ADD_ASSIGN,
            AND,
            ASSIGN,
            BIT_AND,
            BIT_AND_ASSIGN,
            BIT_OR,
            BIT_OR_ASSIGN,
            BIT_XOR,
            BIT_XOR_ASSIGN,
            DIV,
            DIV_ASSIGN,
            EQ,
            GT,
            GT_EQ,
            LT,
            LT_EQ,
            MUL,
            MUL_ASSIGN,
            NOT_EQ,
            OF_ADD,
            OF_ADD_ASSIGN,
            OF_MUL,
            OF_MUL_ASSIGN,
            OF_SUB,
            OF_SUB_ASSIGN,
            OR,
            RANGE_EXCL,
            RANGE_INCL,
            REM,
            REM_ASSIGN,
            SHL,
            SHL_ASSIGN,
            SHR,
            SHR_ASSIGN,
            SUB,
            SUB_ASSIGN,
            ;

            override fun toString(): kotlin.String {
                return when (this) {
                    ADD -> "+"
                    ADD_ASSIGN -> "+="
                    AND -> "&&"
                    ASSIGN -> "="
                    BIT_AND -> "&"
                    BIT_AND_ASSIGN -> "&="
                    BIT_OR -> "|"
                    BIT_OR_ASSIGN -> "|="
                    BIT_XOR -> "^"
                    BIT_XOR_ASSIGN -> "^="
                    DIV -> "/"
                    DIV_ASSIGN -> "/="
                    EQ -> "=="
                    GT -> ">"
                    GT_EQ -> ">="
                    LT -> "<"
                    LT_EQ -> "<="
                    MUL -> "*"
                    MUL_ASSIGN -> "*="
                    NOT_EQ -> "!="
                    OF_ADD -> "+%"
                    OF_ADD_ASSIGN -> "+%="
                    OF_MUL -> "*%"
                    OF_MUL_ASSIGN -> "*%="
                    OF_SUB -> "-%"
                    OF_SUB_ASSIGN -> "-%="
                    OR -> "||"
                    RANGE_EXCL -> ".."
                    RANGE_INCL -> "..="
                    REM -> "%"
                    REM_ASSIGN -> "%="
                    SHL -> "<<"
                    SHL_ASSIGN -> "<<="
                    SHR -> ">>"
                    SHR_ASSIGN -> ">>="
                    SUB -> "-"
                    SUB_ASSIGN -> "-="
                }
            }
        }
    }

    data class UnaryOp(
        val kind: S<Kind>,
        val arg: Expr,
    ): Expr() {
        enum class Kind {
            ADDR_OF,
            DEREF,
            NEG,
            NOT,
            PANICKING_UNWRAP,
            PROPAGATING_UNWRAP,
            ;

            override fun toString(): kotlin.String {
                return when (this) {
                    ADDR_OF -> "&"
                    DEREF -> "*"
                    NEG -> "-"
                    NOT -> "not"
                    PANICKING_UNWRAP -> "!"
                    PROPAGATING_UNWRAP -> "?"
                }
            }
        }
    }

    data class UnnamedStruct(
        /// Whether the unnamed struct literal had `0:` specifier.
        val tuple: S<Unit>?,
        val fields: List<StructLiteralField>,
    ): Expr()

    data class Range(
        val inclusive: S<Unit>?,
        val start: Expr?,
        val end: Expr?,
    ): Expr()

    data class Loop(val body: Block): Expr()

    data class While(
        val cond: Expr,
        val body: Block,
    ): Expr()

    data class For(
        val pattern: S<Pattern>,
        val iterable: Expr,
    ): Expr()

    data class ControlFlow(
        val kind: Kind,
        val label: S<Ident>?,
        val value: Expr?,
    ): Expr() {
        enum class Kind {
            BREAK,
            CONTINUE,
            RETURN,
        }
    }

    data class Selector(
        val value: Expr,
        val name: S<Ident>?,
    ): Expr()

    data class If(
        val cond: Expr,
        val if_true: Block,
        val if_false: Block?,
    ): Expr()

    data class Index(
        val value: Expr,
        val index: Expr,
    ): Expr()

    data class As(
        val expr: Expr,
        val type: TypeExpr,
    ): Expr()

    class FnCall(
        val callee: Callee,
        val args: List<Arg>
    ): Expr() {
        sealed class Callee {
            data class Free(val fn: Expr)
            data class Method(val name: Path)
        }

        data class Arg(
            val name: S<Ident>?,
            val value: Expr,
        )
    }

    data class Let(val def: LetDef): Expr()

    data class Path(val value: czar.syntax.hir.Path): Expr()
}

data class StructLiteralField(
    val name: S<Ident>,
    val value: Expr,
): Node()

data class Use(
    val pub: S<Pub>?,
    val path: Path,
): Node()

data class Impl(
    val type_params: List<TypeParam>,
    val trait: Path?,
    val for_: TypeExpr?,
    val items: List<Item>,
): ModuleItem() {
    sealed class Item {
        data class FnDef(val value: czar.syntax.hir.FnDef): Item()
    }
}

data class TypeAlias(
    val pub: S<Pub>?,
    val name: S<Ident>,
    val typeParams: List<TypeParam>,
    val type: TypeExpr,
): ModuleItem()

data class Hir(
    val root: Module,
    val spans: Map<Node.Id<*>, Span>)