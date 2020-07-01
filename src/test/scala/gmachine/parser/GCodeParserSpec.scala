package gmachine.parser

import org.specs2.Specification
import gmachine.parser.GCodeParser._

final class GCodeParserSpec extends Specification {
  def is =
    s2"""
      GCodeParser should
        $parse_global_start_func
        $parse_main_func
        $parse_combine_inst
    """

  private def parse_global_start_func =
    parseGCode(
      """GLOBSTART Func1
        | PUSH 1
        |   /* some comment */
        | GET
        | SLIDE 2""".stripMargin).getOrElse(List()) mustEqual List(GlobStart("Func", 1), Push(1), Get, Slide(2))

  private def parse_main_func =
    parseGCode(
      """BEGIN
        | PUSHBOOL TRUE
        |   EVAL
        |   PUSHGLOBAL fdds
        | END""".stripMargin).getOrElse(List()) mustEqual List(Begin, PushBool(true), Eval, PushGlobal("fdds"), End)

  private def parse_combine_inst =
    parseGCode(
      """GLOBSTART Func1
        |  /* some comment */
        | PUSH 1
        | GET SLIDE 2
        | GLOBSTART Func2
        |   MKAP UPDATE 1
        | BEGIN
        | PUSHBOOL TRUE
        |   EVAL PUSHGLOBAL fdds
        | END""".stripMargin).getOrElse(List()) mustEqual List(GlobStart("Func", 1), Push(1), Get, Slide(2),
      GlobStart("Func", 2), MkAp, Update(1), Begin, PushBool(true), Eval, PushGlobal("fdds"), End)

}