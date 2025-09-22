package demo.example3

import demo._

class AddOne extends Module {
  val in  = IO(Input(UInt("in", 8)))
  val out = IO(Output(UInt("out", 8)))
}

class AddTwo extends Module {
  val src = IO(Input(UInt("src", 8)))
  val dst = IO(Output(UInt("dst", 8)))

  val i0 = Module(new AddOne, "i0")
  val i1 = Module(new AddOne, "i1")

  i0.in := src
  i1.in := i0.out
  dst   := i1.out
}
