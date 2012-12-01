package diyfea

/** 2D bounding box / bounding rectangle. */
sealed trait BBox {
  def pointWithin(x: Double, y: Double): Boolean
  def includePoint(x: Double, y: Double): BBox
  def includeBBox(b: BBox): BBox = {
    b match {
      case r: RectBBox =>
        this.includePoint(r.minx, r.miny).includePoint(r.maxx, r.maxy)
      case NullBBox => this
    }
  }
    
}

object BBox {
  def apply(): BBox = NullBBox
}

case object NullBBox extends BBox {
  def pointWithin(x: Double, y: Double): Boolean = false
  def includePoint(x: Double, y: Double): BBox = RectBBox(x, x, y, y)
}

case class RectBBox(minx: Double, maxx: Double, miny: Double, maxy: Double) 
  extends BBox
{
  def pointWithin(x: Double, y: Double): Boolean =
    (minx <= x) && (maxx >= x) && (miny <= y) && (maxy >= y)
  def includePoint(x: Double, y: Double): BBox =
    new RectBBox(
      if (x < minx) x else minx,
      if (x > maxx) x else maxx,
      if (y < miny) y else miny,
      if (y > maxy) y else maxy
    )
}