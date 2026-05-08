import yafl.SourceFile
import yafl.parser.{nextToken, Token}

final class LexerTests extends munit.FunSuite:

  test("error"):
    val input = SourceFile("test", "$")
    val token = input.nextToken(input.start)
    assert(token.map((t) => t.tag == Token.error).getOrElse(false))

  test("underscore"):
    val input = SourceFile("test", "_")
    val token = input.nextToken(input.start)
    assert(token.map((t) => t.tag == Token.underscore).getOrElse(false))

  test("equal"):
    val input = SourceFile("test", "=")
    val token = input.nextToken(input.start)
    assert(token.map((t) => t.tag == Token.equal).getOrElse(false))

  test("arrows"):
    val input = SourceFile("test", "-> =>")
    val found = tokens(input).map((t) => t.tag)
    val expected = IArray(
      Token.thinArrow,
      Token.thickArrow)
    assert(found.sameElements(expected))

  test("keywords"):
    val input = SourceFile("test", "fix let if _abc then true else false")
    val found = tokens(input).map((t) => t.tag)
    val expected = IArray(
      Token.fix,
      Token.let,
      Token.`if`,
      Token.identifier,
      Token.`then`,
      Token.boolean,
      Token.`else`,
      Token.boolean)
    assert(found.sameElements(expected))

  test("integer literals"):
    val input = SourceFile("test", "0 123 12")
    val found = tokens(input).map((t) => t.tag)
    val expected = IArray(
      Token.integer,
      Token.integer,
      Token.integer)
    assert(found.sameElements(expected))

  test("punctuation"):
    val input = SourceFile("test", "[]().,:")
    val found = tokens(input).map((t) => t.tag)
    val expected = IArray(
      Token.leftBracket,
      Token.rightBracket,
      Token.leftParenthesis,
      Token.rightParenthesis,
      Token.dot,
      Token.comma,
      Token.colon)
    assert(found.sameElements(expected))

  /** Returns a list with all the tokens left in `input`. */
  private def tokens(input: SourceFile): List[Token] =
    def loop(position: Int, accumulator: List[Token]): List[Token] =
      input.nextToken(position) match
        case Some(t) => loop(t.span.end, t :: accumulator)
        case None => accumulator.reverse
    loop(input.start, Nil)

end LexerTests
