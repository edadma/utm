package io.github.edadma.utm

import scala.util.Using

private lazy val blankRegex = """blank:\s*([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val startRegex = """start:\s*([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val haltRegex = """halt:\s*([a-zA-Z0-9_.$-]+)\s*(?://.*)?""".r
private val stateRegex = """\s*([a-zA-Z0-9_.$-]+)\s+([a-zA-Z0-9_.$-]+)\s+(?:(e|n)|p([a-zA-Z0-9_.$-]+))\s+([lrn])\s*(?://.*)?""".r
private val emptyRegex = """\s*(?://.*)?""".r

def build(s: scala.io.Source): Unit =
  var blank = "-"
  var start = ""
  var halt = "h"

  Using(s)(_.getLines).get foreach {
    case blankRegex(b) => blank = b
    case startRegex(s) => start = s
    case haltRegex(h) => halt = h
    case stateRegex(s, t, p1, p2) => println((s, t, p1, p2))
  }
