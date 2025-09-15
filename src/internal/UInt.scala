package demo

case class UInt (
  val name : String,
  val width: Int,
) {
  var dir: Option[String] = None
}
