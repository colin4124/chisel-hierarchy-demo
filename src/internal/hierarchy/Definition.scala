package demo

object Definition {
  def apply[T <: Module](
    proto: => T
  ): T = {
    Module(proto)
  }
}
