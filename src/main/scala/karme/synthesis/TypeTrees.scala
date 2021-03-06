package karme.synthesis

object TypeTrees {
  trait Typed {
    private var _type: Option[TypeTree] = None

    def getType: TypeTree = _type match {
      case None => Untyped
      case Some(t) => t
    }

    def setType(tt: TypeTree): this.type = _type match {
      case None => _type = Some(tt); this
      case Some(ot) if (ot != tt) => sys.error("Resetting type info.")
      case _ => this
    }
  }

  trait FixedType extends Typed {
    val fixedType: TypeTree

    override def getType: TypeTree = fixedType
    override def setType(tt: TypeTree): this.type = this
  }

  sealed abstract class TypeTree

  case object Untyped extends TypeTree
  case object IntType extends TypeTree
  case object BooleanType extends TypeTree
}
