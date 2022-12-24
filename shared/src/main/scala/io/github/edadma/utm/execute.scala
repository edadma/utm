package io.github.edadma.utm

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

def execute(tm: TM): List[String] =
  val tape = new ArrayBuffer[String]
  var pos = 0
  var state = tm.start

  def left(): Unit =
    if pos == 0 then tape.insert(0, tm.blank)
    else pos -= 1

  def right(): Unit =
    if pos == tape.length then tape.append(tm.blank)
    pos += 1

  def erase(): Unit = print(tm.blank)

  def print(sym: String): Unit =
    if pos == tape.length then tape.append(sym)
    else tape(pos) = sym

  def read: String =
    if pos == tape.length then tm.blank
    else tape(pos)

  while state != tm.halt do
    val actions = tm.actions(state)

    actions.find(_.tape == read) match
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

  tape.dropWhileInPlace(_ == tm.blank).reverse.dropWhileInPlace(_ == tm.blank).reverse.toList
