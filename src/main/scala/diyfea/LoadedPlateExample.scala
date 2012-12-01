package diyfea

import math.{cos, Pi, sin}
import scala.collection.breakOut
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Seq
import scala.collection.immutable.Vector
import java.awt.image.BufferedImage
import java.awt.geom.Rectangle2D
import javax.imageio.ImageIO
import java.io.File
import java.awt.Color
import java.awt.BasicStroke
import java.awt.RenderingHints
import java.io.FileWriter

object LoadedPlateExample extends App {

  val renderAnimation = false  // should an animation be rendered, or just one frame?
  val renderOutlines = true    // should the outlines of the elements be rendered?
  
  val E    = 200E3
  val nu   = 0.3
  val mat  = ElasticMaterial(E, nu)
  val FMag = 1.0
  
  println("Running the loaded plate example.")
  println("This example contains a square plate with a circular hole, loaded")
  println("horizontally by a tensile uniform load.  Only 1/4 of the plate is")
  println("is modelled, and symmetric boundary conditions are used.")
  println("Output should be created as PNG file(s) in the top level directory")
  println("of the source tree.")
  
  def setup(nu: Int, nv: Int, L: Double = 100.0, r: Double = 50.0): 
	(IndexedSeq[Node], IndexedSeq[LinPlaneStressElement], Seq[BC], Seq[Force]) 
	= 
  {
    require(nu % 2 == 1, "nu must be odd")
    
    /** Evaluates the loaded plate geometry at coordinates (u,v). */
    def eval(u: Double, v: Double): (Double, Double) = {
      val angle = Pi * u / 2.0
      val c = cos(angle)
      val s = sin(angle)
      if (u <= 0.5) {
        val x = (1.0-v) * r * c + v * L
        val y = (1.0-v) * r * s + 2.0 * u * v * L
        (x,y)
      } else {
        val x = (1.0-v) * r * c + 2.0 * v * L * (1.0-u)
        val y = (1.0-v) * r * s + v * L
        (x,y)
      }
    }

    /** Compute the nodes. */
    val nodes: IndexedSeq[Node] = (for {
      ui <- 0 until nu
      vi <- 0 until nv
      u = ui.toDouble / (nu - 1)
      v = vi.toDouble / (nv - 1)
    } yield {
      val (x,y) = eval(u, v)
      Node(x, y)
    })(breakOut)
    
    /** Compute the elements. */
    val elements: IndexedSeq[LinPlaneStressElement] = (for {
      ui <- 0 until (nu - 1)
      vi <- 0 until (nv - 1)
      n3i = ui * nv + vi
      n2i = n3i + nv
      n4i = n3i + 1
      n1i = n2i + 1
      n1 = nodes(n1i)
      n2 = nodes(n2i)
      n3 = nodes(n3i)
      n4 = nodes(n4i)
    } yield {
      LinPlaneStressElement(n1, n2, n3, n4, mat)
    })(breakOut)
    
    /** Compute the BCs. */
    // y-symmetry
    val bc_ysymm: Seq[BC] = (for {
      vi <- 0 until nv
      n = nodes(vi)
    } yield {
      BC(n, None, Some(0))
    })(breakOut)
    // x-symmetry
    val bc_xsymm: Seq[BC] = (for {
      vi <- 0 until nv
      ni = (nu-1) * nv + vi
      n = nodes(ni)
    } yield {
      BC(n, Some(0), None)
    })(breakOut)
    val bcs: Seq[BC] = bc_ysymm ++ bc_xsymm
    
    /** Compute the nodal forces. */
    val fx = FMag
    val forces: Seq[Force] = (for {
      ui <- 0 until (nu / 2)
      ei = (nv - 2) + (nv - 1) * ui
      e = elements(ei)
    } yield {
      EdgeTraction(e, 4, fx, 0.0)
    })(breakOut)
    
    (nodes, elements, bcs, forces)
  }
    
  val (nuStart, nuStop, nuStep) = if (renderAnimation) {
    (5, 21, 2)
  } else {
    (21, 22, 1)
  }
  
  var frame: Int = 0  
  for {
    nu <- Range(nuStart, nuStop, nuStep)
    nv: Int = nu / 2
  } {

    val fileName = "./f%04d.png" format frame
    frame = frame + 1
    
	/** Setup the problem, and compute FEA solution */
	val (nodes, elements, bcs, forces) = setup(nu, nv)
	val fea = FEAProblem(nodes, elements, bcs, forces)
	  
	/** Render the output */
	// image parameters
	val imageSize = 1024
	val minX: Double = -10
	val maxX: Double = 110
	val minY: Double = -10
	val maxY: Double = 110
	// create image
	val image = new BufferedImage(imageSize, imageSize, BufferedImage.TYPE_INT_ARGB)
	// generate RenderQuadSets for all elements
	val renderQuadSeq: Seq[RenderQuadSet] = for {
	  e <- elements
    } yield {
	  val i = LinPlaneStressElementInspector(e, fea)
	  RenderQuad.generateSet(i.xy, i.vonMises)
	}
	val renderQuads: RenderQuadSet = RenderQuad.combineSets(renderQuadSeq)
	// render von Mises stresses
	val lut = RainbowLUT(0, 6.5)
	for {
	  xi <- 0 until image.getWidth
	  yi <- 0 until image.getHeight
	  x = (xi / (image.getWidth-1.0)) * (maxX - minX) + minX
	  yprime = image.getHeight - 1 - yi
	  y = (yprime / (image.getHeight-1.0)) * (maxY - minY) + minY
	} {
	  val vm: Option[Double] = renderQuads.sample(x, y)
	  vm.map { vm: Double =>
	    val c = lut(vm)
	    image.setRGB(xi, yi, c.getRGB)
	  }
	}
	// render element outlines
	if (renderOutlines) {
	  val g = {
	    val g = image.createGraphics()
	    val rect = new Rectangle2D.Double(minX, minY, maxX-minX, maxY-minY)
	    val sx = image.getWidth.toDouble / rect.getWidth
	    val sy = image.getHeight.toDouble / rect.getHeight
	    g.scale(1, -1)
	    g.translate(0, -image.getHeight)
	    g.scale(sx, sy)
	    g.translate(-rect.getX, -rect.getY)
	    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
	    g
	  }
	  val deformScale = 4E3
	  val thinStroke = new BasicStroke(0.1f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
	  val thickStroke = new BasicStroke(0.2f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
	  for (e <- elements) {
	    val r = LinPlaneStressElementRenderer(e, fea, image, g, deformScale)
	    g.setStroke(thinStroke)
	    g.setPaint(new Color(0,0,0,0.2f))
	    r.drawUndeformedOutline()
	    g.setStroke(thickStroke)
	    g.setPaint(Color.blue)
	    r.drawDeformedOutline()
	  }
	  g.dispose()
	}
	// write the image as a PNG file
	ImageIO.write(image, "PNG", new File(fileName))    
  }
  
}
