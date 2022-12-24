package io.github.edadma.utm

@main def run(): Unit =
  val input =
    """
      |blank: -
      |start: q1
      |halt: h
      |q1 - p0 r q2
      |q2 - e r q3
      |q3 - p1 r q4
      |q4 - e r h
      |""".trim.stripMargin
  val m = build(scala.io.Source.fromString(input))

  println(m)
  println(execute(m))
