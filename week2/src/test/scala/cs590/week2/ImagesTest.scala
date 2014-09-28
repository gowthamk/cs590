package cs590.week2

import org.scalatest.FunSuite

class ImagesTest extends FunSuite with Images with Codegen {

    def allRed(p: Point): Color = p match { case (x,y) => (x, Times(Const(0.4), y), Const(0), Const(1)) }

  test("allRed") {

    // NOTE: we can write this shorter as
    //
    //     def allRed(p: Point): Color = p match { case (x,y) => (x, 0.5 * y, 0, 1) }
    //
    // thanks to operator overloading (def *(that: DoubleE) = ...) and implicit conversion
    // from Double to DoubleE (implicit def unit(x: Double) = ...).

    generateImage("allRed.html", allRed)
  }

  def checkers(p: Point): Color =p match { case (x,y) => 
      {
        var row = Floor(y*8)
        var col = Floor(x*8)
        var test = Even(row + col)
        val rgb = ITE(test,0,1)
        (rgb,rgb,rgb,1)
      }
    }

  test("checkers") {
    generateImage("checkers.html", checkers)
  }

  test("allRed horizontal flip") {
    generateImage("allRed2.html", (p:Point) => allRed (hflip(p)))
  }

  test("allRed vertical flip") {
    generateImage("allRed3.html", (p:Point) => allRed (vflip(p)))
  }

  test("checkers rotate") {
    generateImage("checkers2.html", (p:Point) => checkers
    (rotate(Pi()/4)(p)))
  }

  test("checkers scale") {
    generateImage("checkers3.html", (p:Point) => checkers
    (scale(2,2)(p)))
  }

  test("checkers translate") {
    generateImage("checkers4.html", (p:Point) => checkers
    (translate(2,2)(p)))
  }

}
