package demo

case class UInt (
  val name : String,
  val width: Int,
) {
  var dir: Option[String] = None
  var connect: Option[UInt] = None

  def := (that: UInt) = {
    this.connect = Some(that)
    that.connect = Some(this)
  }
}
