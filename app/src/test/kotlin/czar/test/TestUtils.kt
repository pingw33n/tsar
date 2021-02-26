package czar.test

import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.TimeUnit

val REPO_DIR: Path = run {
    val dir = Path.of(execute("git rev-parse --show-toplevel").trim())
    check(Files.isDirectory(dir))
    dir
}

val TEST_DIR: Path = REPO_DIR.resolve("app/src/test/resources")

fun execute(cmd: String): String {
    val proc = ProcessBuilder(*cmd.split(" ").toTypedArray())
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
        .redirectError(ProcessBuilder.Redirect.INHERIT)
        .start()

    proc.waitFor(1, TimeUnit.MINUTES)
    check(proc.exitValue() == 0)
    return proc.inputStream.bufferedReader().readText()
}