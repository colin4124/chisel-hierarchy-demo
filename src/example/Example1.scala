package demo.example1

import demo._

class AddOne extends Module {
  val in  = IO(Input(UInt("in", 8)))
  val out = IO(Output(UInt("out", 8)))
}
