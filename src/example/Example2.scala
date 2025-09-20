package demo.example2

import demo._

class AddOne extends Module {
  val in  = IO(Input(UInt("in", 8)))
  val out = IO(Output(UInt("out", 8)))
}

class AddTwo extends Module {
  val src = IO(Input(UInt("src", 8)))
  val dst = IO(Output(UInt("dst", 8)))

  val i0 = Module(new AddOne, "i0")

  i0.in := src
  dst   := i0.out
}
