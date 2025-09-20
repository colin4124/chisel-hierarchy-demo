package demo

abstract class Command

case class DefInstance(id: Module) extends Command

case class Port(id: UInt)
case class DefModule(id: Module)
