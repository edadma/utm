package io.github.edadma.utm

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Using

private lazy val blankRegex = """blank:\s*([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val startRegex = """start:\s*([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val haltRegex = """halt:\s*([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val stateRegex = """\s*([a-zA-Z0-9_.$-]+)\s+([a-zA-Z0-9_.$-]+)\s+(?:(e|n)|p([a-zA-Z0-9_.$-]+))\s+([lrn])\s+([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val emptyRegex = """\s*(?://.*)?""".r

def build(s: scala.io.Source): TM =
  var blank = "-"
  var start = ""
  var halt = "h"
  val actions = new mutable.HashMap[String, ListBuffer[Case]]

  Using(s)(_.getLines).get foreach {
    case blankRegex(b) => blank = b
    case startRegex(s) => start = s
    case haltRegex(h) => halt = h
    case stateRegex(s, t, p1, p2, m, n) =>
      val motion =
        m match
          case "l" => Motion.Left
          case "r" => Motion.Right
          case "n" => Motion.None
      val print =
        (p1, p2) match
          case ("e", _) => Erase
          case ("n", _) => NoPrint
          case (null, s) => SymbolPrint(s)
      val c = Case(t, print, motion, n)

      actions get s match
        case None => actions(s) = ListBuffer(c)
        case Some(l) => l += c
  }

  TM(blank, start, halt, actions.view.mapValues(_.toList).toMap)

trait Print
case object NoPrint extends Print
case object Erase extends Print
case class SymbolPrint(sym: String) extends Print

enum Motion:
  case None, Left, Right

case class Case(tape: String, print: Print, motion: Motion, state: String)

case class TM(blank: String, start: String, halt: String, actions: Map[String, List[Case]])
