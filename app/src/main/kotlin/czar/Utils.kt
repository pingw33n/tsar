package czar

fun unreachable(): Nothing {
    throw AssertionError("Unreachable code")
}

fun <K, V> MutableMap<K, V>.setOnce(k: K, v: V) {
    check(this.put(k, v) == null)
}