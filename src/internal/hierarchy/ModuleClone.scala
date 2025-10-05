package demo

class ModuleClone[T <: Module](val proto: T) extends Module with IsClone[T] {
  override val name = proto.name
  val ioMap: Map[UInt, UInt] = {
    (proto.ports map { p =>
       val clone_port = p.id.cloneTypeFull
       ports += Port(clone_port)
       p.id -> clone_port
    }).toMap
  }
}
