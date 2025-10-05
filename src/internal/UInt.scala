package demo

case class UInt (
  val name : String,
  val width: Int,
) {
  var dir: Option[String] = None
  var connect: Option[UInt] = None
  val parent: Option[Module] = Driver.currentModule

  def fullName = s"${parent.get.inst_name}_$name"

  def is_same_parent(that: UInt) = {
    (this.parent, that.parent) match {
      case (Some(lhs_ctx), Some(rhs_ctx)) =>
        (lhs_ctx.parent, rhs_ctx.parent) match {
          case (Some(lhs_p), Some(rhs_p)) =>
            lhs_p == rhs_p
          case _ =>
            false
        }
      case _ =>
        false
    }
  }

  def := (that: UInt) = {
    if (is_same_parent(that)) {
      val node = UInt(s"${that.fullName}_to_${fullName}", width)

      this.connect = Some(node)
      that.connect = Some(node)

      Driver.currentModule match {
        case Some(m: Module) =>
          DefVar(node) +=: m.cmds
        case _ =>
      }
    } else {
      this.connect = Some(that)
      that.connect = Some(this)
    }
  }

  def cloneTypeFull: this.type = {
    val clone = new UInt(name, width).asInstanceOf[this.type]
    clone.dir = dir
    clone
  }
}
