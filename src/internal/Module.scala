package demo

import scala.collection.mutable.ArrayBuffer

class Module {
  val parent: Option[Module] = Driver.currentModule

  Driver.currentModule = Some(this)

  val name  = this.getClass.getName.split("\\.").last
  val ports = ArrayBuffer[Port]()
  val cmds  = ArrayBuffer[Command]()

  var inst_name = ""
  
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
  def apply[T <: Module](m: => T, inst_name: String = ""): T = {
    val parent = Driver.currentModule

    val module = m

    Driver.currentModule = parent

    parent match {
      case Some(p) =>
        p.cmds += DefInstance(module)
        module.inst_name = inst_name
      case None =>
    }

    Driver.ir += module.gen_ir

    module
  }
}
