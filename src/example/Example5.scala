package demo.example5

import demo._

@instantiable
class AddOne extends Module {
  @public val in  = IO(Input(UInt("in", 8)))
  @public val out = IO(Output(UInt("out", 8)))
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
