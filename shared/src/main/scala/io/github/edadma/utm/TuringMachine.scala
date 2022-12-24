package io.github.edadma.utm

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Using
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

private lazy val blankRegex = """blank:\s*([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val startRegex = """start:\s*([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val haltRegex = """halt:\s*([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val stateRegex =
  """\s*([a-zA-Z0-9_.$-]+)\s+([a-zA-Z0-9_.$-]+)\s+(?:(e|n)|p([a-zA-Z0-9_.$-]+))\s+([lrn])\s+([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val emptyRegex = """\s*(?://.*)?""".r

class TuringMachine(s: scala.io.Source):
  val (blank, start, halt, actions) = {
    var blank = "-"
    var start = ""
    var halt = "h"
    val actions = new mutable.HashMap[String, ListBuffer[Case]]

    Using(s)(_.getLines).get foreach {
      case blankRegex(b) => blank = b
      case startRegex(s) => start = s
      case haltRegex(h)  => halt = h
      case stateRegex(s, t, p1, p2, m, n) =>
        val motion =
          m match
            case "l" => Motion.Left
            case "r" => Motion.Right
            case "n" => Motion.None
        val print =
          (p1, p2) match
            case ("e", _)  => Erase
            case ("n", _)  => NoPrint
            case (null, s) => SymbolPrint(s)
        val c = Case(t, print, motion, n)

        actions get s match
          case None    => actions(s) = ListBuffer(c)
          case Some(l) => l += c
    }

    (blank, start, halt, actions.view.mapValues(_.toList).toMap)
  }

  def run: List[String] =
    val tape = new ArrayBuffer[String]
    var pos = 0
    var state = start

    def left(): Unit =
      if pos == 0 then tape.insert(0, blank)
      else pos -= 1

    def right(): Unit =
      if pos == tape.length then tape.append(blank)
      pos += 1

    def erase(): Unit = print(blank)

    def print(sym: String): Unit =
      if pos == tape.length then tape.append(sym)
      else tape(pos) = sym

    def read: String =
      if pos == tape.length then blank
      else tape(pos)

    while state != halt do
      actions(state).find(_.tape == read) match
        case Some(c) =>
          c.print match
            case Erase            => erase()
            case NoPrint          =>
            case SymbolPrint(sym) => print(sym)
          c.motion match
            case Motion.None  =>
            case Motion.Left  => left()
            case Motion.Right => right()

          state = c.state
        case None => sys.error(s"no matching case for state `$state` and tape position $pos with symbol `${tape(pos)}`")

    tape.dropWhileInPlace(_ == blank).reverse.dropWhileInPlace(_ == blank).reverse.toList
  end run

  override def toString: String = s"TuringMachine($actions)"
end TuringMachine

trait Print
case object NoPrint extends Print
case object Erase extends Print
case class SymbolPrint(sym: String) extends Print

enum Motion:
  case None, Left, Right

case class Case(tape: String, print: Print, motion: Motion, state: String)
