package demo

import scala.collection.mutable.ArrayBuffer

class Module {
  val name = this.getClass.getName.split("\\.").last
  val ports = ArrayBuffer[Port]()
  
  def IO(p: UInt): UInt = {
    ports += Port(p)
    p
  }

  def Input(p: UInt): UInt = {
    p.dir = Some("input ")
    p
  }

  def Output(p: UInt): UInt = {
    p.dir = Some("output")
    p
  }
  
  def gen_ir: DefModule = {
    DefModule(this)
  }
}

object Module {
  def apply(m: Module): Module = {
    Driver.ir += m.gen_ir
    m
  }
}
