package diyfea

import math.{atan2, cos, sin, sqrt}
import no.uib.cipr.matrix.DenseVector
import no.uib.cipr.matrix.UpperSymmPackMatrix
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Vector

/** Truss element.
  *
  * @param n1 node 1 of the truss
  * @param n2 node 2 of the truss
  * @param mat material of the truss
  * @param a cross-sectional area of the truss
  */
case class TrussElement(n1: Node, n2: Node, mat: ElasticMaterial, a: Double)
  extends Element
{
  
  def nodes: IndexedSeq[Node] = Vector(n1, n2)

  val dx: Double = n2.x - n1.x
  val dy: Double = n2.y - n1.y
  val angle: Double = atan2(dy, dx)
  val l: Double = sqrt(dx*dx + dy*dy)
  val k: Double = a * mat.e / l  
  
  
  def K: UpperSymmPackMatrix = {
    
    val c = cos(angle)
    val s = sin(angle)
    val cc = k * c * c
    val cs = k * c * s
    val ss = k * s * s
    
    val m = new UpperSymmPackMatrix(4)
    m.set(0,0,cc); m.set(0,1,cs); m.set(0,2,-cc); m.set(0,3,-cs)
    m.set(1,1,ss); m.set(1,2,-cs); m.set(1,3,-ss)
    m.set(2,2,cc); m.set(2,3,cs)
    m.set(3,3,ss)
    
    m
    
  }
  
  def edgeTraction(t: EdgeTraction): DenseVector = {
    throw new NotImplementedError
  }
  
}