package czar.syntax.hir

inline class Ident(val value: CharSequence) {
    init {
        require(value.isNotEmpty())
    }

    override fun toString(): String {
        return value.toString()
    }
}

interface Node

