package diyfea

case class BC(n: Node, x: Option[Double], y: Option[Double]) {
  def isXConstrained: Boolean = x.isDefined
  def isYConstrained: Boolean = y.isDefined
}