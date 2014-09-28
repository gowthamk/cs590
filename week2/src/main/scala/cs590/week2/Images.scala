package cs590.week2


trait Images {

  type Image = Point => Color
  type Point = (DoubleE, DoubleE)
  type Color = (DoubleE, DoubleE, DoubleE, DoubleE)
  type Transform = Point => Point

  abstract class DoubleE {
    def *(that: DoubleE) = Times(this,that)
    def /(that: DoubleE) = Div(this,that)
    def +(that: DoubleE) = Plus(this,that)
    def -(that: DoubleE) = Minus(this,that)
    def <(that: DoubleE):BoolE = Lt(this,that)
    def >(that: DoubleE):BoolE = Gt(this,that)
  }

  case class Const(d: Double) extends DoubleE
  case class Sym(x: String) extends DoubleE
  case class Times(a: DoubleE, b: DoubleE) extends DoubleE
  case class Div(a: DoubleE, b: DoubleE) extends DoubleE
  case class Plus(a: DoubleE, b: DoubleE) extends DoubleE
  case class Minus(a: DoubleE, b: DoubleE) extends DoubleE
  case class Floor(a: DoubleE) extends DoubleE
  case class Sin(a: DoubleE) extends DoubleE
  case class Cos(a: DoubleE) extends DoubleE
  case class Pi() extends DoubleE
  case class ITE(test:BoolE, a:DoubleE, b:DoubleE) extends DoubleE

  implicit def unit(d: Double): DoubleE = Const(d)

  abstract class BoolE {
    def &&(that: BoolE) = Conj(this,that)
  }

  case class BConst(b: Boolean) extends BoolE
  case class Conj(a: BoolE, b: BoolE) extends BoolE
  case class Eq(a: DoubleE, b: DoubleE) extends BoolE 
  case class Lt(a: DoubleE, b: DoubleE) extends BoolE 
  case class Gt(a: DoubleE, b: DoubleE) extends BoolE 
  case class Even(a: DoubleE) extends BoolE

  implicit def unit(b: Boolean): BoolE = BConst(b)

  /** Transformations **/
  def hflip: Transform = 
    (p : Point) => p match {
      case (x,y) => (1 - x, y)
    }

  def vflip: Transform = 
    (p : Point) => p match {
      case (x,y) => (x, 1 - y)
    }

  def rotate(ang:DoubleE):Transform = {
    var s = Sin(ang)
    var c = Cos(ang)
    (p: Point) => p match { case(x,y) =>
      {
        var x1 = x-0.5
        var y1 = y-0.5
        var x2 = (x1*c) - (y1*s)
        var y2 = (y1*c) + (x1*s)
        var x3 = x2+0.5
        var y3 = y2+0.5
        (x3,y3)
      }
    }
  }

  def scale(sx:DoubleE, sy:DoubleE): Transform =
    (p: Point) => p match {
      case (x,y) => (sx*x, sy*y)
    }

  def translate(dx:DoubleE, dy:DoubleE): Transform =
    (p: Point) => p match {
      case (x,y) => (x+dx, y+dy)
    }

}

trait Codegen extends Images {

  def writeFile(name: String, content: String) {
    val out = new java.io.PrintWriter(new java.io.File(name))
    out.write(content)
    out.close()
  }

  def generateImage(fileName: String, image: Image) = writeFile(fileName,template(fileName,image))

  def beval(e: BoolE) : String = e match {
    case BConst(b) => b.toString
    case Conj(a,b) => s"(${beval(a)} && ${beval(b)})"
    case Eq(d1,d2) => s"(${eval(d1)} == ${eval(d2)})"
    case Even(a) => s"(ev(${eval(a)}))"
    case Lt(d1,d2) => s"(${eval(d1)} < ${eval(d2)})"
    case Gt(d1,d2) => s"(${eval(d1)} > ${eval(d2)})"
  }

  def eval(e: DoubleE): String = e match {
    case Sym(x) => x
    case Const(d) => d.toString
    case Times(a,b) => s"(${eval(a)} * ${eval(b)})"
    case Div(a,b) => s"(${eval(a)} / ${eval(b)})"
    case Plus(a,b) => s"(${eval(a)} + ${eval(b)})"
    case Minus(a,b) => s"(${eval(a)} - ${eval(b)})"
    case Floor(a) => s"(fl(${(eval(a))}))"
    case Sin(a) => s"(sin(${(eval(a))}))"
    case Cos(a) => s"(cos(${(eval(a))}))"
    case Pi() => s"(PI)"
    case ITE(test,a,b) => s"(${beval(test)}? ${eval(a)} : ${eval(b)})"
  }

  def template(fileName: String, image: Image) = s"""
    <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
    <html>
    <head>
      <title>CS590: $fileName</title>
      <style>
      canvas { cursor: crosshair; }
      </style>
      <script>
        var fl = Math.floor;
        var sin = Math.sin;
        var cos = Math.cos;
        var PI = Math.PI;
        var ev = function (x) {
          return (x%2 == 0)? true : false
        };
        function drawImage() {
          var w = 300;
          var h = 300;
          var gCanvas = document.getElementById("canvas");

          var gCtx = gCanvas.getContext("2d");
          gCtx.fillRect(0,0,w,h);

          var imageData = gCtx.getImageData(0,0,w,h);

          for (var x = 0; x < imageData.width; x++) {
            for (var y = 0; y < imageData.height; y++) {
              var offset = (y * imageData.width + x) * 4;

              var xs = x/w;
              var ys = y/h;

              var r = 0;
              var g = 0;
              var b = 0;
              var a = 0;

              ${
                val (r,g,b,a) = image((Sym("xs"),Sym("ys")))
                val (rs,gs,bs,as) = (eval(r), eval(g), eval(b), eval(a))
                s"r = $rs*255; g = $gs*255; b = $bs*255; a = $as*255"
              }

              imageData.data[offset] = r;
              imageData.data[offset + 1] = g;
              imageData.data[offset + 2] = b;
              imageData.data[offset + 3] = a;
            }
          }
          gCtx.putImageData(imageData, 0, 0);
        }
      </script>
    </head>
    <body onload="drawImage()">
      <h1>Image: $fileName</h1>
      <div id="container" align="center">
        <canvas id="canvas" width="300" height="300" style="border: 1px solid black;"/>
      </div>
    </body>
    </html>"""


}


/* 
 * The GADT version
 */

trait ImagesPoly {

  // see section 5 in Elliott's paper

  type Image = Point => Color
  type Point = (DoubleE, DoubleE)
  type Color = (DoubleE, DoubleE, DoubleE, DoubleE)

  
  type DoubleE = Exp[Double]
  type BoolE = Exp[Boolean]

  abstract class Exp[T]

  case class Const[T](d: T) extends Exp[T]
  case class Sym[T](x: String) extends Exp[T]
  case class ITE[T](test:BoolE, a:T, b:T) extends Exp[T]
  case class Eq[T](a: T, b: T) extends BoolE 
  case class Times(a: Exp[Double], b: Exp[Double]) extends Exp[Double]
  case class Div(a: Exp[Double], b: Exp[Double]) extends Exp[Double]
  case class Plus(a: DoubleE, b: DoubleE) extends DoubleE
  case class Minus(a: DoubleE, b: DoubleE) extends DoubleE
  case class Floor(a: DoubleE) extends DoubleE
  case class Sin(a: DoubleE) extends DoubleE
  case class Cos(a: DoubleE) extends DoubleE
  case class Even(a: DoubleE) extends BoolE
  case class Pi() extends DoubleE

  implicit def unit[T](d: T) = Const(d)

  implicit class DoubleOps(a: DoubleE) {
    def *(b: DoubleE) = Times(a,b)
    def /(b:DoubleE) = Div(a,b)
    def +(b: DoubleE) = Plus(a,b)
    def -(b: DoubleE) = Minus(a,b)
  }

  /* Smart Constructors */

  def pi():DoubleE = Const(scala.math.Pi)

  def sin(a:DoubleE):DoubleE = a match {
    case (Const(k)) =>  scala.math.sin(k)
    case _ => Sin(a)
  }

  def cos(a:DoubleE):DoubleE = a match {
    case (Const(k)) =>  scala.math.cos(k)
    case _ => Cos(a)
  }

  def floor(a:DoubleE): DoubleE = a match {
    case (Const(k)) => scala.math.floor(k)
    case _ => Floor(a)
  }

  def times(a:DoubleE, b:DoubleE) : DoubleE = (a,b) match {
    case (Const(k1),Const(k2)) => k1*k2
    case (_,_) => Times(a,b)
  }

  def div(a:DoubleE, b:DoubleE) : DoubleE = (a,b) match {
    case (Const(k1),Const(k2)) => k1/k2
    case (_,_) => Div(a,b)
  }

  def plus(a:DoubleE, b:DoubleE) : DoubleE = (a,b) match {
    case (Const(k1),Const(k2)) => k1+k2
    case (_,_) => Plus(a,b)
  }

  def minus(a:DoubleE, b:DoubleE) : DoubleE = (a,b) match {
    case (Const(k1),Const(k2)) => k1-k2
    case (_,_) => Minus(a,b)
  }

  def iTE[T](a:BoolE, b:T, c:T): Exp[T] = a match {
    case Const(true) => b
    case Const(false) => c
    case _ => ITE(a,b,c)
  }

}


trait Codegen2 extends ImagesPoly {

  def writeFile(name: String, content: String) {
    val out = new java.io.PrintWriter(new java.io.File(name))
    out.write(content)
    out.close()
  }

  def generateImage(fileName: String, image: Image) = writeFile(fileName,template(fileName,image))

  def beval(e: BoolE) : String = e match {
    case Eq(d1,d2) => s"(${eval(d1)} == ${eval(d2)})"
    case Even(a) => s"(ev(${deval(a)}))"
  }

  def deval(e: DoubleE): String = e match {
    case Times(a,b) => s"(${deval(a)} * ${deval(b)})"
    case Div(a,b) => s"(${deval(a)} / ${deval(b)})"
    case Plus(a,b) => s"(${deval(a)} + ${deval(b)})"
    case Minus(a,b) => s"(${deval(a)} - ${deval(b)})"
    case Floor(a) => s"(fl(${(deval(a))}))"
    case Sin(a) => s"(sin(${(deval(a))}))"
    case Cos(a) => s"(cos(${(deval(a))}))"
    case Pi() => s"(PI)"
  }

  def eval[T](e: Exp[T]): String = e match {
    case Sym(x) => x
    case Const(d) => d.toString
    case ITE(test,a,b) => s"(${beval(test)}? ${eval(a)} : ${eval(b)})"
    case Even(_)  => beval(e)
    case Eq(_,_) => beval(e)
    case (de:DoubleE) => deval(de)
  }

  def template(fileName: String, image: Image) = s"""
    <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
    <html>
    <head>
      <title>CS590: $fileName</title>
      <style>
      canvas { cursor: crosshair; }
      </style>
      <script>
        var fl = Math.floor;
        var sin = Math.sin;
        var cos = Math.cos;
        var PI = Math.PI;
        var ev = function (x) {
          return (x%2 == 0)? true : false
        };
        function drawImage() {
          var w = 300;
          var h = 300;
          var gCanvas = document.getElementById("canvas");

          var gCtx = gCanvas.getContext("2d");
          gCtx.fillRect(0,0,w,h);

          var imageData = gCtx.getImageData(0,0,w,h);

          for (var x = 0; x < imageData.width; x++) {
            for (var y = 0; y < imageData.height; y++) {
              var offset = (y * imageData.width + x) * 4;

              var xs = x/w;
              var ys = y/h;

              var r = 0;
              var g = 0;
              var b = 0;
              var a = 0;

              ${
                val (r,g,b,a) = image((Sym("xs"),Sym("ys")))
                val (rs,gs,bs,as) = (eval(r), eval(g), eval(b), eval(a))
                s"r = $rs*255; g = $gs*255; b = $bs*255; a = $as*255"
              }

              imageData.data[offset] = r;
              imageData.data[offset + 1] = g;
              imageData.data[offset + 2] = b;
              imageData.data[offset + 3] = a;
            }
          }
          gCtx.putImageData(imageData, 0, 0);
        }
      </script>
    </head>
    <body onload="drawImage()">
      <h1>Image: $fileName</h1>
      <div id="container" align="center">
        <canvas id="canvas" width="300" height="300" style="border: 1px solid black;"/>
      </div>
    </body>
    </html>"""


}

