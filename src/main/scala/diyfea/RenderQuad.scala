package diyfea

import scala.collection.immutable.Seq

class RenderQuadSet(val quads: Seq[RenderQuad]) {
  
  val bbox: BBox = quads.map(_.bbox).reduce(_ includeBBox _)
  
  def sample(x: Double, y: Double): Option[Double] = {
    for (q <- quads) {
      if (q.insideBBox(x, y)) {
        val s = q.sample(x, y)
        if (s.isDefined) {
          return s
        }
      }
    }
    return None
  }
  
}

class RenderQuad(
  x1: Double, y1: Double,
  x2: Double, y2: Double,
  x3: Double, y3: Double,
  x4: Double, y4: Double,
  v1: Double, v2: Double, v3: Double, v4: Double
) {
  private val tri1 = Tri(x1, y1, x2, y2, x3, y3)
  private val tri2 = Tri(x1, y1, x3, y3, x4, y4)

  val bbox: BBox = NullBBox.
  	includeBBox(tri1.bbox).
  	includeBBox(tri2.bbox)  
  
  def insideBBox(x: Double, y: Double): Boolean = bbox.pointWithin(x, y)
  	
  def sample(x: Double, y: Double): Option[Double] = {
    val bc1 = tri1.baryC(x, y)
    lazy val bc2 = tri2.baryC(x, y)
    if (bc1.inside) {
      Some(bc1.interpD(v1, v2, v3))
    } else if (bc2.inside) {
      Some(bc2.interpD(v1, v3, v4))
    } else {
      None
    }
  }
}

object RenderQuad {
  
  /** Generates a set of RenderQuads by evaluating a function.
    *
    * @param f: function to evaluate
    * @param xy: generates (x,y) coordinates at given function parameters
    * @param nu: number of u parametric samples
    * @param nv: number of v parametric samples
    */
  def generateSet(
    xy: (Double, Double) => (Double, Double),      
    f: (Double, Double) => Double,
    nu: Int = 2,
    nv: Int = 2
  ): RenderQuadSet = {
    require(nu >= 2, "nu must be at least 2")
    require(nv >= 2, "nv must be at least 2")
    val un = nu - 1
    val vn = nv - 1
    val s: Seq[RenderQuad] = for {
      ui <- 0 until un
      vi <- 0 until vn
      u1 = 2 * (ui / un) - 1
      v1 = 2 * (vi / vn) - 1
      u2 = 2 * ((ui + 1) / un) - 1
      v2 = v1
      u3 = u2
      v3 = 2 * ((vi + 1) / vn) - 1
      u4 = u1
      v4 = v3      
    } yield {
      val (x1, y1) = xy(u1, v1)
      val (x2, y2) = xy(u2, v2)
      val (x3, y3) = xy(u3, v3)
      val (x4, y4) = xy(u4, v4)
      val f1 = f(u1, v1)
      val f2 = f(u2, v2)
      val f3 = f(u3, v3)
      val f4 = f(u4, v4)
      new RenderQuad(x1, y1, x2, y2, x3, y3, x4, y4, f1, f2, f3, f4)
    }
    new RenderQuadSet(s)
  }
  
  def combineSets(ss: Seq[RenderQuadSet]): RenderQuadSet =
    new RenderQuadSet(ss.map(_.quads).flatten)
}
