package diyfea

class Node private (val x: Double, val y: Double) {
  override def toString: String = "Node(%f, %f)" format(x, y)
}

object Node {
  def apply(x: Double, y: Double): Node = new Node(x, y)
}