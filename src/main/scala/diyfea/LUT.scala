package diyfea

import java.awt.Color

/** Color look up table. */
trait LUT {
  def apply(d: Double): Color
}

/** Rainbow color look up table. */
case class RainbowLUT(min: Double, max: Double) {
  def apply(d: Double): Color = {
    if (d < min || d > max) {
      Color.GRAY
    } else {
      val x: Double = (d - min) / (max - min)
      val b: Double = 
        if (x < 0.25) {
          1.0
        } else if (x < 0.5) {
          1.0 - 4.0 * (x - 0.25)
        } else {
          0.0
        }
      val g: Double =
        if (x < 0.25) {
          4 * x
        } else if (x > 0.75) {
          1.0 - 4.0 * (x - 0.75) 
        } else {
          1.0
        }
      val r: Double = 
        if (x < 0.5) {
          0.0
        } else if (x > 0.75) {
          1.0
        } else {
          4.0 * (x - 0.5)
        }
      val bi: Int = (b * 255.0).toInt
      val gi: Int = (g * 255.0).toInt
      val ri: Int = (r * 255.0).toInt
      new Color(ri, gi, bi)
    }
  }
  
}

/** Banded rainbow look up table. */
case class BandedRainbowLUT(min: Double, max: Double, nbands: Int) {
  private val rbl = RainbowLUT(0, 1)
  def apply(d: Double): Color = {
    val x: Double = (d - min) / (max - min)
    val band = ((x * nbands).toInt) / nbands.toDouble
    rbl(band)
  }
}