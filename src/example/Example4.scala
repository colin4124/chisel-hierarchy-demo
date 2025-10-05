package demo.example4

import demo._

class AddOne extends Module {
  val in  = IO(Input(UInt("in", 8)))
  val out = IO(Output(UInt("out", 8)))
}

object AddOne {
  implicit class dontCareName(___module: Instance[AddOne]) {
    def in  = ___module._lookup((x: AddOne) => x.in)
    def out = ___module._lookup(_.out)
  }
}

class AddTwo extends Module {
  val src = IO(Input(UInt("src", 8)))
  val dst = IO(Output(UInt("dst", 8)))

  val u_addOneDef = Definition(new AddOne)

  val i0 = Instance(u_addOneDef, "i0")
  val i1 = Instance(u_addOneDef, "i1")


  i0.in := src
  i1.in := i0.out
  dst   := i1.out
}
