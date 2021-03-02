package czar.syntax.parse

import czar.diag.Diag
import czar.diag.Report
import czar.syntax.S
import czar.syntax.Source
import czar.syntax.Span
import czar.syntax.hir.Ident
import czar.unreachable
import java.math.BigDecimal
import java.math.BigInteger

private const val EOF = '\u0000'
private const val RAW_MARKER = '#'

private enum class IdentKind {
    NORMAL,
    RAW,
}

private sealed class Mode(val start: Int) {
    var level: Int = 0

    class Normal(start: Int): Mode(start)
    class StringLit(start: Int, val rawLen: Int?): Mode(start)
    class StringLitSubst(start: Int): Mode(start)
}

private class Buf(val nlMode: Lexer.NlMode) {
    companion object {
        const val CAP = 3
    }

    val toks: MutableList<S<Token>> = mutableListOf()
    var lastRemoved: S<Token>? = null

    fun removeFirst(): S<Token> {
        val r = toks.removeAt(0)
        lastRemoved = r
        return r
    }
}

internal class Lexer(val src: Source, val diag: Diag) {
    enum class NlMode {
        ALLOW,
        SKIP,
    }

    private var pos: Int = 0
    private val buf: Buf = Buf(NlMode.ALLOW)
    private val nlessBuf: Buf = Buf(NlMode.SKIP)

    private val modeStack: MutableList<Mode> = mutableListOf()
    init {
        modeStack.add(Mode.Normal(0))
    }
    private val nlModeStack: MutableList<NlMode> = mutableListOf()
    init {
        nlModeStack.add(NlMode.SKIP)
    }

    fun pushNlMode(nlMode: NlMode) {
        nlModeStack.add(nlMode)
    }

    fun popNlMode() {
        nlModeStack.removeLast()
    }

    inline fun <T>withNlMode(nlMode: NlMode, f: () -> T): T {
        pushNlMode(nlMode)
        try {
            return f()
        } finally {
            popNlMode()
        }
    }

    fun next(): S<Token> {
        val r = at(0)
        popBuf()
        return r
    }

    fun push(tok: S<Token>) {
        addBuf(buf, tok, prepend = true)
        addBuf(nlessBuf, tok, prepend = true)
    }

    fun at(i: Int): S<Token> {
        check(i >= -1 && i < Buf.CAP)

        val curBuf = curBuf()
        return if (i == -1) {
            curBuf.lastRemoved!!
        } else {
            fillBufs()
            curBuf.toks[i]
        }
    }

    private fun curBuf(): Buf {
        return when (nlModeStack.last()) {
            NlMode.ALLOW -> buf
            NlMode.SKIP -> nlessBuf
        }
    }

    fun ident(span: Span): Ident {
        val t = text(span)
        val raw = t.length >= 2 && t[1] == RAW_MARKER
        return Ident(if (raw) t.subSequence(2, t.length) else t)
    }

    fun label(span: Span): Ident {
        val t = text(span)
        check(t[0] == '#')
        return Ident(t.subSequence(1, t.length))
    }

    fun stringLit(span: Span): Sequence<CharSequence> {
        val rawLen = if (src.text[span.start] == 'r') {
            var v = 0
            while (v < src.text.length && src.text[span.start + 1 + v] == RAW_MARKER) {
                v += 1
            }
            v
        } else {
            null
        }
        val payload = Span(span.start + if (rawLen == null) 1 else rawLen + 2, span.end)
        check(src.text[payload.start - 1] == '"')
        val t = text(payload)
        return decodeString(t, payload.start, rawLen == null)
    }

    fun stringLitEnd(tok: S<Token>): Sequence<CharSequence> {
        require(tok.value == Token.STRING_LIT_END || tok.value == Token.RAW_STRING_LIT_END)
        val start: Int
        val rawLen = if (tok.value == Token.RAW_STRING_LIT_END) {
            var rawLen = 0
            while (src.text[tok.span.end - rawLen - 1] != '"') {
                rawLen += 1
            }
            start = if (tok.span.length > 1 &&
                    src.text[tok.span.start] == 'r' &&
                    src.text[tok.span.start + rawLen + 1] == '"' &&
                    (1..rawLen).all { src.text[tok.span.start + it] == RAW_MARKER }) {
                rawLen + 2
            } else {
                0
            }
            rawLen
        } else {
            start = if (tok.span.length > 1 && src.text[tok.span.start] == '"') {
                1
            } else {
                0
            }
            null
        }
        val payload = Span(tok.span.start + start, tok.span.end - (rawLen ?: 0) - 1)
        check(src.text[payload.end] == '"')
        val t = text(payload)
        return decodeString(t, payload.start, rawLen == null)
    }

    fun stringLitSubstEnd(span: Span): CharSequence {
        val start = if (src.text[span.start] == ':') {
            1
        } else {
            check(span.length == 1)
            0
        }
        check(src.text[span.end - 1] == '}')
        return text(Span(span.start + start, span.end - 1))
    }

    fun charLit(span: Span): Int {
        val payload = Span(span.start + 1, span.end - 1)
        check(src.text[payload.start - 1] == '\'' && src.text[payload.end] == '\'')
        val t = text(payload)
        val s = decodeString(t, payload.start, true).joinToString("")
        val cps = s.codePoints().iterator()
        if (!cps.hasNext()) {
            error(span, "invalid character literal")
            return 0
        }
        val r = cps.next()
        if (cps.hasNext()) {
            error(span, "invalid character literal")
            return 0
        }
        return r
    }

    fun intLit(span: Span): BigInteger {
        var t = text(span)
        var radix = Radix.DEC
        if (t.length > 2 && t[0] == '0') {
            val r = Radix.of(t[1])
            if (r == null) {
                if (!isDigit(t[1], Radix.DEC)) {
                    fatal(Span(span.start + 1, span.start + 2), "invalid integer literal radix")
                }
            } else {
                t = t.subSequence(2, t.length)
                radix = r
            }
        }

        val s = deunderscore(t, span.start + t.length - span.length, radix)
        val r = s.toString().toBigIntegerOrNull(radix.value)
        return r ?: fatal(span, "invalid integer literal")
    }

    fun floatLit(span: Span): BigDecimal {
        val t = text(span)
        val s = deunderscore(t, span.start, Radix.DEC)
        val r = s.toString().toBigDecimalOrNull()
        return r ?: fatal(span, "invalid float literal")
    }

    private fun decodeString(s: CharSequence, start: Int, unescape: Boolean): Sequence<CharSequence> {
        return sequence {
            var i = 0
            while (i < s.length) {
                val chars = when (s[i]) {
                    '\r' -> {
                        if (s.getOrNull(i + 1) == '\n') {
                            i += 1
                        }
                        "\n"
                    }
                    '\\' -> if (unescape) {
                        val chars = when (s[i + 1]) {
                            'b' -> "\b"
                            'n' -> "\n"
                            'r' -> "\r"
                            't' -> "\t"
                            'u' -> {
                                val (unescaped, inc) = unicodeEscape(start + i + 2, s.subSequence(i + 2, s.length))
                                i += inc
                                unescaped
                            }
                            '\'' -> "'"
                            '"' -> "\""
                            '\\' -> "\\"
                            '{' -> "{"
                            else -> {
                                error(Span(start + i, start + i + 2), "invalid escape")
                                s.subSequence(i + 1, i + 2).toString()
                            }
                        }
                        i += 1
                        chars
                    } else {
                        s.subSequence(i, i + 1)
                    }
                    else -> s.subSequence(i, i + 1)
                }
                i += 1
                if (chars.isNotEmpty()) {
                    yield(chars)
                }
            }
        }
    }

    private fun unicodeEscape(start: Int, s: CharSequence): Pair<CharSequence, Int> {
        if (s[0] != '{') {
            error(Span.one(start), "expected '{'")
            return Pair("", 0)
        }

        var endMarker = 1
        while (endMarker < s.length && isDigit(s[endMarker], Radix.HEX)) {
            endMarker += 1
        }
        if (endMarker == 1) {
            error(Span(start + 1, start + endMarker + 1), "expected unicode code point")
            return Pair("", 0)
        }
        if (endMarker == s.length) {
            error(Span(start, start + endMarker), "unterminated unicode escape")
            return Pair("", 0)
        }
        if (s[endMarker] != '}') {
            error(Span(start + endMarker, start + endMarker + 1), "expected '}'")
            return Pair("", 0)
        }

        val span = Span(start + 1, start + endMarker)

        val cpStr = s.subSequence(1, endMarker)
        val cp = cpStr.toString().toLongOrNull(16)
        if (endMarker - 1 > 6 || cp == null || cp < 0 || cp > 0x10ffff || cp in 0xd800..0xdfff) {
            error(span, "invalid unicode code point")
            return Pair(cpStr, endMarker + 1)
        }
        val res = String(Character.toChars(cp.toInt()))
        return Pair(res, endMarker + 1)
    }

    private fun mode(): Mode {
        return modeStack.last()
    }

    private fun text(span: Span): CharSequence {
        return src.text.subSequence(span.start, span.end)
    }

    private fun nthChar(i: Int): Char {
        return src.text.getOrNull(pos + i) ?: EOF
    }

    private fun nextChar(): Char {
        val r = nthChar(0)
        if (pos < src.text.length) {
            pos += 1
        }
        return r
    }

    private fun wasLastNl(): Boolean {
        return buf.toks.isNotEmpty() && buf.toks.last().value == Token.NL ||
                buf.toks.isEmpty() && buf.lastRemoved?.value == Token.NL
    }

    private fun fillBufs() {
        while (curBuf().toks.size < Buf.CAP) {
            val tok = scan()
            if (tok == null || tok.value == Token.NL && wasLastNl()) {
                continue
            }
            addBuf(buf, tok, prepend = false)
            addBuf(nlessBuf, tok, prepend = false)
        }
    }

    private fun addBuf(buf: Buf, tok: S<Token>, prepend: Boolean) {
        if (buf.nlMode == NlMode.ALLOW || tok.value != Token.NL) {
            buf.toks.add(if (prepend) 0 else buf.toks.size, tok)
        }
    }

    private fun popBuf() {
        val sync: Buf
        val pop = when (nlModeStack.last()) {
            NlMode.ALLOW -> {
                sync = nlessBuf
                buf
            }
            NlMode.SKIP -> {
                sync = buf
                nlessBuf
            }
        }

        val removed = pop.removeFirst()
        while (sync.toks.isNotEmpty() && sync.toks[0].span.start <= removed.span.start) {
            sync.removeFirst()
        }
    }

    private fun scan(): S<Token>? {
        if (mode() is Mode.StringLit) {
            return if (nthChar(0) == '{') {
                val tok = S(Span(pos, pos + 1), Token.STRING_LIT_SUBST_START)
                nextChar()
                modeStack.add(Mode.StringLitSubst(tok.span.start))
                tok
            } else {
                stringLitNext()
            }
        }

        val start = pos
        var identKind: IdentKind? = null
        var tok2: Token? = null
        val tok = when (val c = nextChar()) {
            EOF -> Token.EOF
            '{' -> {
                mode().level += 1
                Token.BRACE_OPEN
            }
            '}' -> {
                when (val mode = mode()) {
                    is Mode.Normal -> {
                        mode().level -= 1
                        Token.BRACE_CLOSE
                    }
                    is Mode.StringLit -> unreachable()
                    is Mode.StringLitSubst -> {
                        if (mode.level == 0) {
                            modeStack.removeLast()
                            check(mode() is Mode.StringLit)
                            Token.STRING_LIT_SUBST_END
                        } else {
                            Token.BRACE_CLOSE
                        }
                    }
                }
            }
            '[' -> {
                mode().level += 1
                when (nthChar(0)) {
                    '=' -> {
                        nextChar()
                        Token.BRACKET_EQ
                    }
                    else -> Token.BRACKET_OPEN
                }
            }
            ']' -> {
                mode().level -= 1
                Token.BRACKET_CLOSE
            }
            '(' -> {
                mode().level += 1
                Token.PAREN_OPEN
            }
            ')' -> {
                mode().level -= 1
                Token.PAREN_CLOSE
            }
            '&' -> when (nthChar(0)) {
                '&' -> {
                    nextChar()
                    Token.AMP2
                }
                '=' -> {
                    nextChar()
                    Token.AMP_EQ
                }
                else -> Token.AMP
            }
            '!' -> when (nthChar(0)) {
                '=' -> {
                    nextChar()
                    Token.BANG_EQ
                }
                else -> Token.BANG
            }
            ':' -> {
                val mode = mode()
                when (mode) {
                    is Mode.Normal -> when (nthChar(0)) {
                        ':' -> {
                            nextChar()
                            Token.COLON2
                        }
                        else -> Token.COLON
                    }
                    is Mode.StringLit -> unreachable()
                    is Mode.StringLitSubst -> {
                        if (mode.level == 0) {
                            val end = src.text.indexOf('}', pos)
                            if (end == -1) {
                                fatal(Span(mode.start, src.text.length), "unterminated string subst")
                            }
                            pos = end + 1
                            modeStack.removeLast()
                            check(mode() is Mode.StringLit)
                            Token.STRING_LIT_SUBST_END
                        } else {
                            Token.COLON
                        }
                    }
                }
            }
            ',' -> Token.COMMA
            '.' -> when (nthChar(0)) {
                '.' -> {
                    nextChar()
                    when (nthChar(0)) {
                        '.' -> {
                            nextChar()
                            Token.DOT3
                        }
                        '=' -> {
                            nextChar()
                            Token.DOT2_EQ
                        }
                        else -> Token.DOT2
                    }
                }
                else -> Token.DOT
            }
            '=' -> when (nthChar(0)) {
                '>' -> {
                    nextChar()
                    Token.EQ_GT
                }
                '=' -> {
                    nextChar()
                    Token.EQ2
                }
                else -> Token.EQ
            }
            '<' -> when (nthChar(0)) {
                '<' -> {
                    nextChar()
                    when (nthChar(0)) {
                        '=' -> {
                            nextChar()
                            Token.LT2_EQ
                        }
                        else -> Token.LT2
                    }
                }
                '=' -> {
                    nextChar()
                    Token.LT_EQ
                }
                else -> Token.LT
            }
            '>' -> when (nthChar(0)) {
                '>' -> {
                    nextChar()
                    when (nthChar(0)) {
                        '=' -> {
                            nextChar()
                            Token.GT2_EQ
                        }
                        else -> Token.GT2
                    }
                }
                '=' -> {
                    nextChar()
                    Token.GT_EQ
                }
                else -> Token.GT
            }
            ';' -> Token.SEMI
            '|' -> when (nthChar(0)) {
                '|' -> {
                    nextChar()
                    Token.PIPE2
                }
                '=' -> {
                    nextChar()
                    Token.PIPE_EQ
                }
                else -> Token.PIPE
            }
            '-' -> when (nthChar(0)) {
                '=' -> {
                    nextChar()
                    Token.DASH_EQ
                }
                '>' -> {
                    nextChar()
                    Token.DASH_GT
                }
                '%' -> {
                    nextChar()
                    when (nthChar(0)) {
                        '=' -> {
                            nextChar()
                            Token.DASH_PERCENT_EQ
                        }
                        else -> Token.DASH_PERCENT
                    }
                }
                else -> Token.DASH
            }
            ifChar(c) { tok2 = numberLit(c); tok2 != null } -> tok2!!
            '+' -> when (nthChar(0)) {
                '=' -> {
                    nextChar()
                    Token.PLUS_EQ
                }
                '%' -> {
                    nextChar()
                    when (nthChar(0)) {
                        '=' -> {
                            nextChar()
                            Token.PLUS_PERCENT_EQ
                        }
                        else -> Token.PLUS_PERCENT
                    }
                }
                else -> Token.PLUS
            }
            '^' -> when (nthChar(0)) {
                '=' -> {
                    nextChar()
                    Token.HAT_EQ
                }
                else -> Token.HAT
            }
            '?' -> Token.QUEST
            '*' -> when (nthChar(0)) {
                '=' -> {
                    nextChar()
                    Token.STAR_EQ
                }
                '%' -> {
                    nextChar()
                    when (nthChar(0)) {
                        '=' -> {
                            nextChar()
                            Token.STAR_PERCENT_EQ
                        }
                        else -> Token.STAR_PERCENT
                    }
                }
                else -> Token.STAR
            }
            '%' -> when (nthChar(0)) {
                '=' -> {
                    nextChar()
                    Token.PERCENT_EQ
                }
                else -> Token.PERCENT
            }
            '\n' -> Token.NL
            '\r' -> {
                if (nthChar(0) == '\n') {
                    nextChar()
                }
                Token.NL
            }
            ifChar(c) { lineComment(c) } -> return null
            ifChar(c) { blockComment(c, start) } -> return null
            '/' -> when (nthChar(0)) {
                '=' -> {
                    nextChar()
                    Token.SLASH_EQ
                }
                else -> Token.SLASH
            }
            ifChar(c) {
                if (isWhitespace(c)) {
                    advanceWhile(::isWhitespace)
                    true
                } else {
                    false
                }
            } -> return null
            ifChar(c) { tok2 = stringLitStart(c, start); tok2 != null } -> tok2!!
            ifChar(c) { charLit(c, start) } -> Token.CHAR_LIT
            ifChar(c) {
                identKind = ident(c)
                identKind != null
            } -> keywordOrIdent(start, identKind!!)
            '#' -> {
                label(start)
                Token.LABEL
            }
            else -> {
                if (isEof()) {
                    Token.EOF
                } else {
                    fatal(Span(start, pos), "invalid character")
                }
            }
        }
        return S(Span(start, pos), tok)
    }

    private enum class FloatPart {
        NONE,
        FRAC,
        EXP_START,
        EXP_MIDDLE,
    }

    private fun numberLit(c: Char): Token? {
        if (!isDigit(c, Radix.DEC)) {
            return null
        }
        val radix = if (c == '0') {
            val r = Radix.of(nthChar(0))
            if (r != null) {
                nextChar()
            }
            r
        } else {
            null
        } ?: Radix.DEC

        var floatPart = FloatPart.NONE
        var dotPos: Int? = null
        while (true) {
            when (nthChar(0)) {
                'e', 'E' -> if (radix == Radix.DEC) {
                    if (floatPart < FloatPart.EXP_START) {
                        floatPart = FloatPart.EXP_START
                        when (nthChar(1)) {
                            '+', '-' -> {
                                nextChar()
                                floatPart = FloatPart.EXP_MIDDLE
                            }
                        }
                    }
                }
                '.' -> {
                    if (floatPart == FloatPart.NONE) {
                        val next = nthChar(1)
                        if (isDigit(next, Radix.DEC)) {
                            floatPart = FloatPart.FRAC
                            dotPos = pos
                        } else {
                            // 0.abs
                            // 0..10
                            break
                        }
                    } else {
                        // 0.1.0
                        // 0.1.abs
                        break
                    }
                }
                in '0'..'9' -> {
                    if (floatPart == FloatPart.EXP_START) {
                        floatPart = FloatPart.EXP_MIDDLE
                    }
                }
                in 'A'..'Z', in 'a'..'z', '_' -> {}
                else -> break
            }
            nextChar()
        }
        return when (floatPart) {
            FloatPart.NONE -> Token.INT_LIT
            FloatPart.FRAC -> {
                pos = dotPos!!
                Token.INT_LIT
            }
            else -> Token.FLOAT_LIT
        }
    }

    private fun deunderscore(s: CharSequence, start: Int, radix: Radix): CharSequence {
        val r = StringBuilder()
        var i = 0
        while (i < s.length) {
            if (s[i] == '_') {
                if (i > 0 && (!isDigit(s[i - 1], radix) || !isDigit(s.getOrElse(i + 1) { ' ' }, radix))) {
                    error(Span.one(start + i), "invalid underscore placement")
                    // Skip successive _'s
                    while (s.getOrNull(i + 1) == '_') {
                        i += 1
                    }
                }
            } else {
                r.append(s[i])
            }
            i += 1
        }
        return r
    }

    private fun charLit(c: Char, start: Int): Boolean {
        if (c != '\'') {
            return false
        }

        scanChars(charLit = true, false, null, start)

        return true
    }

    private fun keywordOrIdent(start: Int, identKind: IdentKind): Token {
        val span = Span(start, pos)
        val ident = ident(span)
        if (identKind == IdentKind.RAW) {
            when (ident.value) {
                "_", "self", "Self" -> error(span, "`${ident.value}` can't be a raw identifier")
            }
            return Token.IDENT
        }
        return when (ident.value) {
            "break" -> Token.KW_BREAK
            "continue" -> Token.KW_CONTINUE
            "false" -> Token.KW_FALSE
            "fn" -> Token.KW_FN
            "for" -> Token.KW_FOR
            "in" -> Token.KW_IN
            "impl" -> Token.KW_IMPL
            "loop" -> Token.KW_LOOP
            "mut" -> Token.KW_MUT
            "pub" -> Token.KW_PUB
            "self" -> Token.KW_SELF_LOWER
            "Self" -> Token.KW_SELF_UPPER
            "trait" -> Token.KW_TRAIT
            "true" -> Token.KW_TRUE
            "type" -> Token.KW_TYPE
            "while" -> Token.KW_WHILE
            "as" -> {
                when (nthChar(0)) {
                    '!' -> {
                        nextChar()
                        Token.KW_AS_BANG
                    }
                    '%' -> {
                        nextChar()
                        Token.KW_AS_PERCENT
                    }
                    else -> Token.KW_AS
                }
            }
            "const" -> Token.KW_CONST
            "else" -> Token.KW_ELSE
            "enum" -> Token.KW_ENUM
            "if" -> Token.KW_IF
            "is" -> Token.KW_IS
            "match" -> Token.KW_MATCH
            "module" -> Token.KW_MODULE
            "not" -> Token.KW_NOT
            "package" -> Token.KW_PACKAGE
            "ret" -> Token.KW_RET
            "static" -> Token.KW_STATIC
            "struct" -> Token.KW_STRUCT
            "super" -> Token.KW_SUPER
            "_" -> Token.KW_UNDERSCORE
            "unsafe" -> Token.KW_UNSAFE
            "use" -> Token.KW_USE
            "where" -> Token.KW_WHERE
            else -> Token.IDENT
        }
    }

    private fun ident(c: Char): IdentKind? {
        if (!isIdentStart(c)) {
            return null
        }
        val kind = if (c == 'r' && nthChar(0) == RAW_MARKER) {
            nextChar()
            IdentKind.RAW
        } else {
            IdentKind.NORMAL
        }
        advanceWhile { isIdentMiddle(it) }
        return kind
    }

    private fun label(start: Int) {
        if (!isIdentStart(nextChar())) {
            fatal(Span(start, pos), "invalid label")
        }
        advanceWhile { isIdentMiddle(it) }
        if (pos - start == 2 && src.text[pos - 1] == '_') {
            error(Span(start, pos), "invalid label")
        }
    }

    private fun stringLitStart(c: Char, start: Int): Token? {
        val rawLen = if (c == 'r' && (nthChar(0) == '"' ||
                    nthChar(0) == RAW_MARKER && (nthChar(1) == RAW_MARKER || nthChar(1) == '"'))) {
            var v = 0
            advanceUntil {
                if (nthChar(0) != '"') {
                    v += 1
                    null
                } else {
                    1
                }
            }
            v
        } else if (c == '"') {
            null
        } else {
            return null
        }

        modeStack.add(Mode.StringLit(start, rawLen))

        return scanString()
    }

    private fun stringLitNext(): S<Token> {
        val start = pos
        val tok = scanString()
        return S(Span(start, pos), tok)
    }

    private fun scanString(): Token {
        val mode = mode() as Mode.StringLit
        val substs = mode.rawLen == null

        return scanChars(charLit = false, substs, mode.rawLen, mode.start)
    }

    private fun scanChars(charLit: Boolean, substs: Boolean, rawLen: Int?, start: Int): Token {
        val delim = if (charLit) '\'' else '"'
        while (true) {
            if (substs && nthChar(0) == '{') {
                return if (charLit) Token.CHAR_LIT else Token.STRING_LIT
            }
            when (nextChar()) {
                EOF -> fatal(Span(start, pos), "unterminated ${if (charLit) "character" else "string"} literal")
                delim -> {
                    val end = rawLen == null || rawLen == 0 ||
                        ((rawLen - 1) downTo 0).all { nthChar(it) == RAW_MARKER }
                    if (end) {
                        pos += rawLen ?: 0
                        return if (charLit) {
                            Token.CHAR_LIT
                        } else {
                            modeStack.removeLast()
                            if (rawLen == null) Token.STRING_LIT_END else Token.RAW_STRING_LIT_END
                        }
                    }
                }
                '\\' -> if (rawLen == null) {
                    // Skip \u{
                    if (nextChar() == 'u' && nthChar(0) == '{') {
                        nextChar()
                    }

                }
                '\r' -> {
                    val p = pos - 1
                    if (nthChar(0) == '\n') {
                        nextChar()
                    }
                    if (charLit) {
                        error(Span.one(p), "newline must be escaped inside character literal")
                    }
                }
                '\n' -> if (charLit) {
                    error(Span.one(pos - 1), "newline must be escaped inside character literal")
                }
            }
        }
    }

    private fun isEof(): Boolean {
        return pos == src.text.length
    }

    private fun advanceWhile(f: (Char) -> Boolean) {
        while (true) {
            if (isEof()) {
                break
            }
            if (!f(nthChar(0))) {
                break
            }
            nextChar()
        }
    }

    private fun advanceUntil(f: () -> Int?): Boolean {
        while (true) {
            if (isEof()) {
                return false
            }
            val i = f()
            if (i == null) {
                nextChar()
            } else {
                check(i >= 0 && pos + i <= src.text.length)
                pos += i
                return true
            }

        }
    }

    private fun ifChar(c: Char, expected: () -> Boolean): Char {
        return if (expected()) {
            c
        } else {
            EOF
        }
    }

    private fun lineComment(first: Char): Boolean {
        if (first != '/' || nthChar(0) != '/') {
            return false
        }

        nextChar()

        advanceWhile { it != '\n' && it != '\r' }

        return true
    }

    private fun blockComment(first: Char, start: Int): Boolean {
        if (first != '/' || nthChar(0) != '*') {
            return false
        }

        nextChar()

        var depth = 1
        while (true) {
            val c = nextChar()
            val c2 = nthChar(0)
            when {
                c == '/' && c2 == '*' -> {
                    nextChar()
                    depth += 1
                }
                c == '*' && c2 == '/' -> {
                    nextChar()
                    depth -= 1
                    if (depth == 0) {
                        break
                    }
                }
                c == EOF -> break
            }
        }

        if (depth != 0) {
            fatal(Span(start, pos), "unterminated comment")
        }

        return true
    }

    private fun fatal(span: Span, msg: String): Nothing {
        error(span, msg)
        throw ParseException()
    }

    private fun error(span: Span, msg: String) {
        diag.report(Report(src, span, msg))
    }
}

private fun isWhitespace(c: Char): Boolean {
    return when (c) {
        '\u0009', // \t
        '\u000B', // vertical tab
        '\u000C', // form feed
        '\u0020', // space
        -> true
        else -> false
    }
}

private enum class Radix(val value: Int) {
    BIN(2),
    DEC(10),
    HEX(16),
    ;

    companion object {
        fun of(c: Char): Radix? {
            return when (c) {
                'b' -> BIN
                'x' -> HEX
                else -> null
            }
        }
    }
}

private fun isDigit(c: Char, radix: Radix): Boolean {
    return when (radix) {
        Radix.BIN -> c in '0'..'1'
        Radix.DEC -> c in '0'..'9'
        Radix.HEX -> c in '0'..'9' || c in 'a'..'f' || c in 'A'..'F'
    }
}

private fun isIdentStart(c: Char): Boolean {
    return when (c) {
        in 'a'..'z', in 'A'..'Z', '_' -> true
        else -> false
    }
}

private fun isIdentMiddle(c: Char): Boolean {
    return when (c) {
        in 'a'..'z', in 'A'..'Z', in '0'..'9', '_' -> true
        else -> false
    }
}
