package sierpinski

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.timers._
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.Canvas

@JSExport
object Sierpinski {
  val fact = 0.866025403
  val smallestDim = scala.math.min(dom.window.innerWidth, dom.window.innerHeight)
  val totWdt:Int = (0.9 * smallestDim).toInt
  val totHgt:Int = (fact*totWdt).toInt

  var animating = false
  

  // Fractal related methods
  abstract class Triangle
  case class FullTriangle(w:Double, h:Double) extends Triangle
  case class PiercedTriangle(inTriangle:Triangle) extends Triangle
  
  def buildSierpinskiTriangle(n:Int) = {
    if (n > 6) {
      val alpha = 0.44*(n-3)
      buildSierpinskiTriangle_(n, (totWdt*alpha).toInt, (totHgt*alpha).toInt)
    } else
      buildSierpinskiTriangle_(n, totWdt, totHgt)
  }
  def buildSierpinskiTriangle_(n: Int, w:Double, h:Double):Triangle = n match {
    case 0 => FullTriangle(w, h)
    case _ => {
      val newTriangle = buildSierpinskiTriangle_(n-1,w/2,h/2)
      PiercedTriangle(newTriangle)
    }
  }
    
  def triangleHgt(t:Triangle):Double = t match {
    case FullTriangle(_,h) => h
    case PiercedTriangle(inTriangle) => 2*triangleHgt(inTriangle)
  }
  
  def triangleWdt(t:Triangle):Double = t match {
    case FullTriangle(w,_) => w
    case PiercedTriangle(inTriangle) => 2*triangleWdt(inTriangle)
  }

  // End fractal related methods


  @JSExport
  def genTriangleBtnClicked(): Unit = {

    if (animating)
      return
    
    val canvas = dom.document.getElementById("canvas").asInstanceOf[Canvas]

    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.clearRect(0, 0, canvas.width, canvas.height);

    // Begin utility function declarations
    def fillBlackUpwardTriangle(rootX: Int, rootY: Int, base:Int, height:Int): Unit = {

      val botLeftX = rootX-base/2
      val botLeftY = rootY+height
      
      val botRightX = rootX+base/2
      val botRightY = botLeftY

      ctx.fillStyle = "black"
      ctx.beginPath()
      ctx.moveTo(rootX,rootY)

      ctx.lineTo(botLeftX,botLeftY)
      ctx.lineTo(botRightX,botRightY)
      ctx.closePath() 
      //ctx.stroke()
      ctx.fill()
    }

    def fillWhiteDownwardTriangle(rootX: Int, rootY: Int, base:Int, height:Int): Unit = {

      val topLeftX = rootX-base/2
      val topLeftY = rootY-height
      
      val topRightX = rootX+base/2
      val topRightY = topLeftY

      ctx.fillStyle = "white"
      ctx.beginPath()
      ctx.moveTo(rootX,rootY)

      ctx.lineTo(topLeftX,topLeftY)
      ctx.lineTo(topRightX,topRightY)
      ctx.closePath() 
      //ctx.stroke()
      ctx.fill()
    }

    def drawSierpinski(t:Triangle): Unit = {
      val startX = (dom.window.innerWidth/1.9).toInt
      val startY = (dom.window.innerHeight/20).toInt

      fillBlackUpwardTriangle(startX, startY, triangleWdt(t).toInt, triangleHgt(t).toInt)

      pierceSierpinski(t, startX, startY)

    }

    def pierceSierpinski(t:Triangle, curX:Int, curY:Int): Unit = {
      t match {
        case FullTriangle(_, _) => () // We are done here
        case PiercedTriangle(inTriangle) => {
          // Pierce this triangle
          fillWhiteDownwardTriangle(curX, curY+triangleHgt(t).toInt, triangleWdt(inTriangle).toInt, triangleHgt(inTriangle).toInt)

          // Pierce subtriangles
          // top triangle
          pierceSierpinski(inTriangle, curX, curY)

          // bottom triangles
          val delta = (triangleWdt(inTriangle)/2).toInt
          val midTriangleY = curY+triangleHgt(inTriangle).toInt
          pierceSierpinski(inTriangle, curX-delta, midTriangleY)
          pierceSierpinski(inTriangle, curX+delta, midTriangleY)
        }
           
      }
    }
    
    def drawAllSierpinksisUpTo(n:Int) {
      
      def drawAndPostpone(i:Int) {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        val t  = buildSierpinskiTriangle(i)
        drawSierpinski(t)

        if (i < n) {
          setTimeout(700) { drawAndPostpone(i+1) }
        } else { animating = false }
      }

      animating = true
      drawAndPostpone(0)
    }

    // End utility function declarations

    val numInput = dom.document.getElementById("numIterations").asInstanceOf[html.Input]
    val n = numInput.value.toInt

    val allTrianglesCheckbox = dom.document.getElementById("allValues").asInstanceOf[html.Input]
    val shouldShowAllTriangles = allTrianglesCheckbox.checked

    if (shouldShowAllTriangles) {
      drawAllSierpinksisUpTo(n)
    } else {
      val t  = buildSierpinskiTriangle(n)
      drawSierpinski(t)
    }
  }

    
}
