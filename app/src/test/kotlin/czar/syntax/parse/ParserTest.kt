package czar.syntax.parse

import czar.diag.Diag
import czar.syntax.FILE_SUFFIX
import czar.syntax.Source
import czar.test.AutoExpect
import czar.test.TEST_DIR
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.DynamicTest.dynamicTest
import org.junit.jupiter.api.TestFactory
import org.junit.jupiter.api.extension.ExtendWith
import java.nio.file.Files
import java.nio.file.Path
import kotlin.streams.asSequence

@ExtendWith(AutoExpect::class)
class ParserTest {
    @TestFactory
    internal fun ok(): List<DynamicTest> {
        return findInputs(Path.of("parseOk"))
            .map { inp ->
                dynamicTest("${inp.fileName}") {
                    val (hir, errs) = parse(inp)
                    assertEquals(errs, "")
                    AutoExpect.verify(inp.fileName.toString(), hir!!)
                }
            }
            .toList()
    }

    @TestFactory
    internal fun fail(): List<DynamicTest> {
        return findInputs(Path.of("parseFail"))
            .map { inp ->
                dynamicTest("${inp.fileName}") {
                    val (_, errs) = parse(inp)
                    assertTrue(errs.isNotEmpty())
                    AutoExpect.verify(inp.fileName.toString(), errs)
                }
            }
            .toList()

    }

    private fun findInputs(dir: Path): Sequence<Path> {
        return Files.list(TEST_DIR.resolve(dir))
            .asSequence()
            .filter { Files.isRegularFile(it) && it.fileName.toString().endsWith(".$FILE_SUFFIX") }
    }

    private fun parse(inp: Path): Pair<String?, String> {
        val diag = Diag(inp.parent)
        val hir = parse(Source(Files.readString(inp), inp), diag)
        assertTrue(hir != null || diag.reports.isNotEmpty())
        val hirStr = if (hir != null) {
            hir.printer().copy(relativizePath = TEST_DIR).toString()
        } else {
            null
        }
        return Pair(hirStr, diag.toString())
    }
}