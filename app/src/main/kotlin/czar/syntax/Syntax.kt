package czar.syntax

import java.nio.file.Path

const val FILE_SUFFIX: String = "cz"

data class Span(val start: Int, val end: Int) {
    val length: Int get() = end - start

    companion object {
        fun empty(): Span = Span(0, 0)

        fun one(start: Int): Span = Span(start, start + 1)

        fun joinSpanneds(items: List<S<*>>): Span? {
            return if (items.isEmpty()) {
                null
            } else {
                Span(items[0].span.start, items[items.lastIndex].span.end)
            }
        }
    }

    init {
        require(start <= end)
    }

    fun isEmpty(): Boolean = length == 0
}

data class S<T>(val span: Span, val value: T) {
    inline fun <U>map(f: (T) -> U): S<U> = S(span, f(value))
}

data class Source(
    val text: CharSequence,
    val location: Path,
)