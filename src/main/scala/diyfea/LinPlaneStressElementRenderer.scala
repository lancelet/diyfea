package diyfea

import java.awt.image.BufferedImage
import java.awt.geom.Rectangle2D
import java.awt.Graphics2D
import java.awt.geom.GeneralPath

case class LinPlaneStressElementRenderer(
  e: LinPlaneStressElement,
  fea: FEAProblem,
  image: BufferedImage,
  g: Graphics2D,
  deformScale: Double = 1E3
){

  val inspector = LinPlaneStressElementInspector(e, fea)
  
  def drawUndeformedOutline() = {
    val path = new GeneralPath()
    path.moveTo(e.n1.x, e.n1.y)
    path.lineTo(e.n2.x, e.n2.y)
    path.lineTo(e.n3.x, e.n3.y)
    path.lineTo(e.n4.x, e.n4.y)
    path.closePath()
    g.draw(path)
  }
  
  def drawDeformedOutline() = {
    val path = new GeneralPath()
    val i = inspector
    val s = deformScale
    path.moveTo(e.n1.x + s * i.u1._1, e.n1.y + s * i.u1._2)
    path.lineTo(e.n2.x + s * i.u2._1, e.n2.y + s * i.u2._2)
    path.lineTo(e.n3.x + s * i.u3._1, e.n3.y + s * i.u3._2)
    path.lineTo(e.n4.x + s * i.u4._1, e.n4.y + s * i.u4._2)
    path.closePath()
    g.draw(path)
  }
    
}