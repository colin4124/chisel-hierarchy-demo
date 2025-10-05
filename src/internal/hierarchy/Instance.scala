package demo

class Instance[A] (val underlying: IsClone[A]) {
  def _lookup(
    that: A => UInt
  ): UInt = {
    val ret = that(underlying.proto)
    underlying match {
      case x: ModuleClone[_] => x.ioMap(ret)
    }
  }
}

object Instance {
  def apply[T <: Module](m: T, name: String): Instance[T] = {

    val c = Module(new ModuleClone(m))
    c.inst_name = name

    new Instance(c)
  }
}
