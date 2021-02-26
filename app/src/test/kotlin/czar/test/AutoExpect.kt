package czar.test

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.extension.AfterEachCallback
import org.junit.jupiter.api.extension.BeforeEachCallback
import org.junit.jupiter.api.extension.ExtensionContext
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.TimeUnit

class AutoExpect private constructor(): AfterEachCallback, BeforeEachCallback {
    private enum class Mode {
        VERIFY,
        RECORD_ALL,
        RECORD_NEW,
    }

    private data class Context(
        val dir: Path,
        val id: String,
        val mode: Mode,
    )

    companion object {
        private val REPO_DIR: Path by lazy {
            val dir = Path.of(execute("git rev-parse --show-toplevel").trim())
            check(Files.isDirectory(dir))
            dir
        }
        private val CONTEXT: ThreadLocal<Context> = ThreadLocal()

        fun verify(subId: String, actual: CharSequence) {
            verify0(subId, actual)
        }

        fun verify(actual: CharSequence) {
            verify0(null, actual)
        }

        private fun verify0(subId: String?, actual: CharSequence) {
            val ctx = CONTEXT.get()
            val file = if (subId == null) {
                ctx.dir.resolve(ctx.id + ".txt")
            } else {
                ctx.dir.resolve(ctx.id).resolve("$subId.txt")
            }

            val expected = if (Files.exists(file)) Files.readString(file) else null
            when (ctx.mode) {
                Mode.VERIFY -> {
                    Assertions.assertNotNull(expected, "$file doesn't exist")
                    Assertions.assertEquals(expected, actual)
                }
                Mode.RECORD_ALL, Mode.RECORD_NEW -> {
                    if (expected == null || ctx.mode == Mode.RECORD_ALL && actual != expected) {
                        Files.createDirectories(file.parent)
                        Files.writeString(file, actual)
                        execute("git add $file")
                    } else {
                        Assertions.assertEquals(expected, actual)
                    }
                }
            }
        }

        private fun execute(cmd: String): String {
            val proc = ProcessBuilder(*cmd.split(" ").toTypedArray())
                .redirectOutput(ProcessBuilder.Redirect.PIPE)
                .redirectError(ProcessBuilder.Redirect.INHERIT)
                .start()

            proc.waitFor(1, TimeUnit.MINUTES)
            check(proc.exitValue() == 0)
            return proc.inputStream.bufferedReader().readText()
        }
    }

    override fun beforeEach(context: ExtensionContext?) {
        check(CONTEXT.get() == null)
        context!!
        val modeStr = System.getenv("AUTO_EXPECT")
        val mode = when (modeStr) {
            null, "verify" -> Mode.VERIFY
            "record_new" -> Mode.RECORD_NEW
            "record_all" -> Mode.RECORD_ALL
            else -> throw IllegalArgumentException("Unknown AUTO_EXPECT option: $modeStr")
        }
        val axDir = REPO_DIR.resolve("app/src/test/resources/autoexpect");
        check(Files.isDirectory(axDir))
        val dir = axDir.resolve(context.testClass.get().name
            .removePrefix("czar.")
            .replace('.', '/'))
        val id = context.testMethod.get().name.toString()
            .removeSuffix("\$app")
        CONTEXT.set(Context(dir, id, mode))
    }

    override fun afterEach(context: ExtensionContext?) {
        check(CONTEXT.get() != null)
        CONTEXT.remove()
    }
}