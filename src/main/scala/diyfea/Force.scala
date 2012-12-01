package diyfea

sealed trait Force

case class ConcentratedForce(n: Node, fx: Double, fy: Double) extends Force
case class EdgeTraction(e: Element, edge: Int, fx: Double, fy: Double)
  extends Force