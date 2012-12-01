package diyfea

/** Barycentric coordinates within a triangle. */
case class BaryC(l1: Double, l2: Double, l3: Double) {
  val inside: Boolean = 
    (0 <= l1 && l1 <= 1 && 0 <= l2 && l2 <= 1 && 0 <= l3 && l3 <= 1)
    
  def interpD(v1: Double, v2: Double, v3: Double): Double =
    v1 * l1 + v2 * l2 + v3 * l3  
}

/** 2D triangle. */
case class Tri(
  x1: Double, y1: Double,
  x2: Double, y2: Double,
  x3: Double, y3: Double
) {

  // bounding box
  val bbox: BBox = {
    BBox().includePoint(x1, y1).
           includePoint(x2, y2).
           includePoint(x3, y3)
  }
  
  // Checks if a point is inside the bounding box of the triangle
  def insideBBox(x: Double, y: Double): Boolean = bbox.pointWithin(x, y)

  // Constants used in the barycentric coordinate calculation
  private val detT = (x1 - x3)*(y2 - y3) - (x2 - x3)*(y1 - y3)
  private val c1 = y2 - y3
  private val c2 = x3 - x2
  private val c3 = y3 - y1
  private val c4 = x1 - x3
  
  // Returns barycentric coordinates of an (x,y) coordinate
  def baryC(x: Double, y: Double): BaryC = {
    val dx = x - x3
    val dy = y - y3
    val l1 = (c1*dx + c2*dy) / detT
    val l2 = (c3*dx + c4*dy) / detT
    val l3 = 1.0 - l1 -l2
	BaryC(l1, l2, l3)
  }
  
}