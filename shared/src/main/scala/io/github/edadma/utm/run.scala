package io.github.edadma.utm

import scala.collection.mutable.ArrayBuffer

def run(tm: TM): List[String] =
  val tape = new ArrayBuffer[String]
  var state = tm.start

  while state != tm.halt do
    val actions = tm.actions(state)


