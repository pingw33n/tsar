package czar

fun unreachable(): Nothing {
    throw AssertionError("Unreachable code")
}

fun <K, V> MutableMap<K, V>.setOnce(k: K, v: V) {
    check(this.put(k, v) == null)
}

class Checkpointed<T: Checkpointed.State<T>>(initial: T) {
    private val states: MutableList<T> = mutableListOf(initial)

    val state
        get() = states.last()

    interface State<T: State<T>> {
        fun copy(): T
    }

    fun checkpoint() {
        states.add(state.copy())
    }

    fun rollback() {
        states.removeLast()
    }

    fun commit() {
        val i = states.size - 2
        states[i] = states.removeLast()
    }
}