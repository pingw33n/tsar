package czar.syntax.hir.print

import czar.syntax.S
import czar.syntax.Span
import czar.syntax.hir.*
import czar.unreachable
import kotlin.reflect.KClass

data class Printer(val hir: Hir, val relativizePath: java.nio.file.Path? = null) {
    fun print(out: Appendable) {
        PrinterImpl(hir, relativizePath, out).print()
    }

    override fun toString(): String {
        val s = StringBuilder()
        print(s)
        return s.toString()
    }
}

private const val INDENT: Int = 2

private sealed class Value {
    object List: Value()
    data class Literal(val value: CharSequence): Value()
    object Object: Value()
    data class String(val value: CharSequence): Value()
}

private class PrinterImpl(val hir: Hir, val relativizePath: java.nio.file.Path?, val out: Appendable) {
    private var levels: MutableList<Value> = mutableListOf()
    private var sep: Boolean = false

    fun print() {
        module(hir.root)
        check(levels.isEmpty())
    }

    private fun module(module: Module) {
        val (source, name, items) = module

        obj(span = span(module)) {
            if (source != null) {
                val (_, location) = source
                val path = if (relativizePath == null) {
                    location
                } else {
                    relativizePath.toAbsolutePath().relativize(location.toAbsolutePath())
                }
                entry("source", value = Value.String(path.toString()))
            }
            if (name != null) {
                val (mName, mPub) = name.value
                obj("name", span = name.span) {
                    ident("name", mName)
                    pub(mPub)
                }
            }

            list("items", items = items) {
                moduleItem(it)
            }
        }
    }

    private fun moduleItem(value: ModuleItem) {
        obj(type = value::class, span = span(value)) {
            when (value) {
                is FnDef -> fnDef(value)
                is Impl -> TODO()
                is Module -> TODO()
                is StructDef -> TODO()
                is TypeAlias -> TODO()
                is Use -> TODO()
            }::class.java
        }
    }

    private fun fnDef(fnDef: FnDef) {
        val (pub,
            name,
            typeParams,
            params,
            result,
            unsafe,
            body,
            variadic,
        ) = fnDef
        pub(pub)
        ident("name", name)

        typeParams(typeParams)

        list("params", items = params) {
            obj(span = span(it)) {
                val (label, pName, type) = it
                if (label != null) {
                    entry("label", span = label.span, value = Value.Literal(label.value.value))
                }
                ident("name", pName)
                typeExpr("type", type)
            }
        }

        if (result != null) {
            typeExpr("result", result)
        }
        if (unsafe != null) {
            entry("unsafe", span = unsafe.span)
        }
        if (variadic) {
            entry("variadic")
        }
        if (body != null) {
            block("body", body)
        }
    }

    private fun block(field: String, block: Block) {
        list(field, items = block.items) { item ->
            obj(type = item::class) {
                when (item) {
                    is Block.Item.Expr -> expr(value = item.value)
                    is Block.Item.ModuleItem -> moduleItem(item.value)
                }::class
            }
        }
    }

    private fun typeExpr(field: String? = null, value: TypeExpr) {
        obj(field, value::class, span(value)) {
            when (value) {
                is TypeExpr.Path -> path(value = value.value)
                is TypeExpr.Ref -> typeExpr(value = value.value)
                is TypeExpr.Slice -> {
                    typeExpr("item", value.item)
                    if (value.len != null) {
                        expr("len", value.len)
                    }
                    Unit
                }
                is TypeExpr.UnnamedStruct -> {
                    list("fields", items = value.value.fields) { it ->
                        obj {
                            val (pub, name, type) = it
                            pub(pub)
                            ident("name", name)
                            typeExpr("type", type)
                        }
                    }
                }
            }::class
        }
    }

    private fun expr(field: String? = null, value: Expr) {
        obj(field, value::class, span(value)) {
            when (value) {
                is Expr.As -> {
                    val (expr, type) = value
                    expr("expr", expr)
                    typeExpr("type", type)
                }
                is Expr.BinaryOp -> {
                    val (kind, left, right) = value
                    entry("kind", span = kind.span, value = Value.Literal(kind.value.name))
                    expr("left", left)
                    expr("right", right)
                }
                is Expr.Bool -> entry(value = Value.Literal(value.value.toString()))
                is Expr.Char -> entry(value = Value.Literal("'\\u{${value.value.toString(16)}}'"))
                is Expr.ControlFlow -> TODO()
                is Expr.Float -> entry(value = Value.Literal(value.value.toString()))
                is Expr.FnCall -> {
                    val (callee, args) = value
                    if (callee != null) {
                        obj("callee", callee::class) {
                            when (callee) {
                                is Expr.FnCall.Callee.Free -> expr(value = callee.value)
                                is Expr.FnCall.Callee.Method -> path(value = callee.name)
                            }
                        }
                    }
                    list("args", items = args) {
                        obj {
                            val (label, v) = it
                            ident("label", label)
                            expr("value", v)
                        }
                    }
                }
                is Expr.For -> TODO()
                is Expr.If -> TODO()
                is Expr.Index -> {
                    val (v, index) = value
                    expr("value", v)
                    expr("index", index)
                }
                is Expr.Int -> entry(value = Value.Literal(value.value.toString()))
                is Expr.Let -> TODO()
                is Expr.Loop -> TODO()
                is Expr.Path -> path(value = value.value)
                is Expr.Range -> {
                    val (inclusive, start, end) = value
                    if (inclusive != null) {
                        entry("inclusive", span = inclusive.span)
                    }
                    if (start != null) {
                        expr("start", start)
                    }
                    if (end != null) {
                        expr("end", end)
                    }
                    Unit
                }
                is Expr.Selector -> {
                    val (v, name) = value
                    expr("value", v)
                    if (name != null) {
                        ident("name", name)
                    }
                    Unit
                }
                is Expr.String -> entry(value = Value.Literal(value.value))
                is Expr.UnaryOp -> {
                    val (kind, arg) = value
                    entry("kind", span = kind.span, value = Value.Literal(kind.value.name))
                    expr("arg", arg)
                }
                is Expr.While -> TODO()
            }::class
        }
    }

    private fun path(field: String? = null, value: Path) {
        obj(field, span = span(value)) {
            val (origin, prefix, suffix) = value
            if (origin != null) {
                obj("origin", origin.value::class, origin.span) {
                    when (origin.value) {
                        is Path.Origin.Package -> {
                            val (name) = origin.value
                            if (name != null) {
                                entry("name", value = Value.Literal(name.value))
                            }
                            Unit
                        }
                        is Path.Origin.Super -> {
                            val (count) = origin.value
                            entry("count", value = Value.Literal(count.toString()))
                        }
                    }::class
                }
            }
            list("prefix", items = prefix) {
                pathItem(value = it)
            }
            list("suffix", items = suffix) { item ->
                when (item.value) {
                    is Path.SuffixItem.Item -> {
                        obj(type = item.value::class, span = item.span) {
                            val (it, renamedAs) = item.value
                            pathItem("item", it)
                            if (renamedAs != null) {
                                ident("renamedAs", renamedAs)
                            }
                        }
                    }
                    Path.SuffixItem.Star -> entry(type = item::class, span = item.span)
                }::class
            }
        }
    }

    private fun pathItem(field: String? = null, value: S<Path.Item>) {
        obj(field, span = value.span) {
            val (name, typeArgs) = value.value
            ident("name", name)
            list("typeArgs", items = typeArgs) { typeArg ->
                typeExpr(value = typeArg)
            }
        }
    }

    private fun typeParams(typeParams: List<S<Ident>>) {
        list("typeParams", items = typeParams) {
            ident(value = it)
        }
    }

    private inline fun <T>list(
        field: String? = null,
        type: KClass<*>? = null,
        span: Span? = null,
        items: Collection<T>,
        f: (T) -> Unit,
    ) {
        if (span != null || items.isNotEmpty()) {
            entry(field, type, span, Value.List)
            for (item in items) {
                f(item)
            }
            end()
        }
    }

    private inline fun obj(field: String? = null, type: KClass<*>? = null, span: Span? = null, f: () -> Unit) {
        entry(field, type, span, Value.Object)
        f()
        end()
    }

    private fun ident(field: String? = null, value: S<Ident>?) {
        if (value != null) {
            entry(field, span = value.span, value = Value.Literal(value.value.value))
        }
    }

    private fun span(node: Node) = hir.spans[node.id]!!

    private fun pub(pub: S<Pub>?) {
        if (pub != null) {
            obj("pub", span = pub.span) {
                if (pub.value.scope != null) {
                    entry("scope", span = pub.value.scope.span, value = Value.Literal(pub.value.scope.value.name))
                }
            }
        }
    }

    private fun entry(field: String? = null, type: KClass<*>? = null, span: Span? = null, value: Value? = null) {
        check(field != null || type != null || value != null)
        beginLine()

        if (field != null) {
            writeSep("$field:")
        }
        if (type != null) {
            writeSep(type.simpleName!!)
        }
        if (span != null) {
            writeSep("@(${span.start}..${span.end})")
        }
        val v = when (value) {
            is Value.List -> {
                levels.add(value)
                "["
            }
            is Value.Literal -> "`${value.value}`"
            Value.Object -> {
                levels.add(value)
                "{"
            }
            is Value.String -> "\"${value.value.toString()
                .replace("\n", "\\n")
                .replace("\r", "\\r")}\""
            null -> null
        }
        if (v != null) {
            writeSep(v)
        }
        writeNl()
    }

    private fun writeNl() {
        out.append('\n')
        sep = true
    }

    private fun beginLine() {
        out.append(" ".repeat(levels.size * INDENT))
        sep = true
    }

    private fun end() {
        val v = levels.removeLast()
        beginLine()
        when (v) {
            is Value.List -> write("]")
            is Value.Object ->  write("}")
            is Value.Literal, is Value.String -> unreachable()
        }
        writeNl()
    }

    private fun write(s: String) {
        out.append(s)
        sep = false
    }

    private fun writeSep(s: String) {
        sep()
        write(s)
    }

    private fun sep() {
        if (!sep) {
            out.append(' ')
            sep = true
        }
    }
}