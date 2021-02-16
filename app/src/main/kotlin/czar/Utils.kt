package czar

fun unreachable(): Nothing {
    throw AssertionError("Unreachable code")
}