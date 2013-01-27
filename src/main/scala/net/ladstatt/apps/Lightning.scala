package net.ladstatt.apps

import scala.collection.JavaConversions.seqAsJavaList
import scala.util.Random
import javafx.animation.FadeTransition
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.event.Event
import javafx.event.EventHandler
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.effect.Bloom
import javafx.scene.effect.GaussianBlur
import javafx.scene.input.MouseEvent
import javafx.scene.layout.BorderPane
import javafx.scene.media.Media
import javafx.scene.media.MediaPlayer
import javafx.scene.paint.Color
import javafx.scene.shape.Line
import javafx.scene.shape.Rectangle
import javafx.stage.Stage
import javafx.util.Duration
import javafx.scene.web.WebView
import scala.math.cos
import scala.math.sin
import scala.math.Pi
import javafx.scene.text.Font
import javafx.scene.text.Text
import javafx.scene.effect.Reflection
import javafx.geometry.Point2D
import javafx.scene.Node
import javafx.animation.Timeline
import javafx.animation.Animation
import javafx.animation.KeyFrame
import javafx.scene.shape.Circle
import scala.collection.mutable.SetBuilder

object Lightning {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Lightning], args: _*)
  }

}

class Lightning extends javafx.application.Application {

  val canvasWidth = 800
  val canvasHeight = 600

  val lightningTime = 1400
  val boltWidth = 6
  val displacement = 100
  val curDetail = 5

  val branchCnt = 3
  def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] { def handle(e: E) = f(e) }
  val mediaResource = new Media(getClass.getResource("/strike.mp3").toString)
  val font = Font.loadFont(getClass.getResourceAsStream("/alph.ttf"), 150)

  def mouseClickHandler(group: Group) = mkEventHandler(
    (e: MouseEvent) => {
      new MediaPlayer(mediaResource).play
      val start = Vec(canvasWidth / 2, canvasHeight / 5)
      val dest = Vec(e.getX, e.getY())
      mkFadingBolt(start, dest, boltWidth, branchCnt, Color.BLANCHEDALMOND, group)
    })

  def mkFadingBolt(start: Vec, dest: Vec, width: Double, branchCnt: Int, col: Color, group: Group) = {
    val boltParts = mkBolt(mkBranches(start, dest, displacement, curDetail, branchCnt), width, col)
    val duration = (Random.nextDouble * lightningTime).toInt
    val bolt = new Group
    bolt.getChildren.addAll(boltParts)
    //    group.getChildren.addAll(withFade(bolt, duration, Random.nextDouble + 0.2))
    group.getChildren.addAll(boltParts.map(withFade(_, duration, Random.nextDouble + 0.2)))
  }

  def mkPointsOnText(t: Node) = {
    def mkRandPoint: Vec = {
      val bounds = t.getBoundsInLocal()
      val x = (Random.nextDouble * bounds.getWidth).toInt + bounds.getMinX
      val y = (Random.nextDouble * bounds.getHeight).toInt + bounds.getMinY
      if (t.contains(x, y)) Vec(x, y) else mkRandPoint
    }

    val (src, dest, _) = (for (i <- 0 to 5) yield {
      val src = mkRandPoint
      val dest = mkRandPoint
      (src, dest, (src - dest).length)
    }).sortWith((a, b) => a._3 < b._3).head

    (src, dest)
  }

  def mkCircle(vec: Vec): Circle = {
    val c = new Circle
    c.setCenterX(vec.x)
    c.setCenterY(vec.y)
    c.setRadius(1)
    c.setStroke(Color.GOLD)
    c.setFill(Color.GOLD)
    c
  }

  def mkTimeline(group: Group, t: Text, color : Color) = {
    val timeline = new Timeline
    timeline.setRate(24)
    timeline.setCycleCount(Animation.INDEFINITE)
    timeline.getKeyFrames().add(
      new KeyFrame(Duration.seconds(2),
        new EventHandler[ActionEvent]() {
          def handle(event: ActionEvent) {
            val (start, dest) = mkPointsOnText(t)
            mkFadingBolt(start, dest, Random.nextInt(8), Random.nextInt(2), color, group)
          }
        }))
    timeline.play()
  }

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Lightning Level 5: Text")
    val t = new Text("LIGHTNING")
    val textColor = Color.WHITESMOKE
    t.setFont(font)
    
    t.setStroke(textColor.brighter)
    t.setFill(textColor.darker)
    val blocal = t.getBoundsInLocal()
    val bparent = t.getBoundsInParent()
    t.setX((canvasWidth - blocal.getWidth()) / 2)
    t.setY((canvasHeight - blocal.getHeight()) / 2)
    val borderPane = new BorderPane()
    val drawingBoard = new Group()
    drawingBoard.getChildren().add({
      val b = new Rectangle(0, 0, canvasWidth, canvasHeight)
      b.setFill(Color.BLACK)
      b.addEventHandler(MouseEvent.MOUSE_CLICKED, mouseClickHandler(drawingBoard))
      b
    })
    mkTimeline(drawingBoard, t, textColor)
    drawingBoard.getChildren().add(t)

    borderPane.setCenter(drawingBoard)
    primaryStage.setScene(new Scene(borderPane, canvasWidth, canvasHeight))
    primaryStage.show()
  }

  def withFade(group: Group, duration: Int, brightness: Double) = {
    val ft = new FadeTransition(Duration.millis(duration), group)
    ft.setFromValue(brightness / 8)
    ft.setToValue(brightness)
    ft.setCycleCount(2)
    ft.setAutoReverse(true)
    ft.setOnFinished(mkEventHandler((e: ActionEvent) => { group.getChildren().clear() }))
    ft.play()
    group
  }

  def mkMidPointReplacement(source: Vec, dest: Vec, displace: Double, curDetail: Double): List[(Vec, Vec)] = {
    if (displace < curDetail) {
      List((source, dest))
    } else {
      val displacedCenter = source.center(dest).displace(displace)
//      val displacedCenter = source.center(dest)
      mkMidPointReplacement(source, displacedCenter, displace / 2, curDetail) ++
        mkMidPointReplacement(displacedCenter, dest, displace / 2, curDetail)
    }
  }

  def mkBranches(origSource: Vec, origDest: Vec, displacement: Double, curDetail: Double, branchCnt: Int): List[(Vec, Vec)] = {

    def mkPoints(source: Vec, dest: Vec, branchCnt: Int): List[(Vec, Vec)] = {
      branchCnt match {
        case 0 => mkMidPointReplacement(source, dest, displacement, curDetail)
        case _ => {
          val listOfVecPairs = mkMidPointReplacement(source, dest, displacement, curDetail)
          val idx = (listOfVecPairs.size / 2).toInt
          val (newStartPos, _) = listOfVecPairs(idx)
          listOfVecPairs ++
            (if (Random.nextInt(3) > 1)
              mkBranches(newStartPos, newStartPos + (dest - newStartPos).spin(3 * Pi / 4), displacement, curDetail, branchCnt - 1)
            else List()) ++
            (if (Random.nextInt(3) > 1)
              mkBranches(newStartPos, newStartPos + (dest - newStartPos).spin(5 * Pi / 4), displacement, curDetail, branchCnt - 1)
            else List())
        }
      }
    }

    mkPoints(origSource, origDest, branchCnt)
  }

  def mkBolt(positions: => List[(Vec, Vec)], width: Double, color: Color): List[Group] =
    (for ((a, b) <- positions) yield mkLine(a, b, width, color)).toList

  case class Vec(x: Double, y: Double) {
    def -(that: Vec) = Vec(that.x - x, that.y - y)
    def +(that: Vec) = Vec(x + that.x, y + that.y)
    def *(factor: Double) = Vec(factor * x, factor * y)
    def /(l: Double) = if (l != 0) Vec(x / l, y / l) else sys.error("div.0")
    def length = scala.math.sqrt(x * x + y * y)
    def displace(f: Double) = Vec(x + (Random.nextDouble - 0.5) * f, y + (Random.nextDouble - 0.5) * f)
    def onedir = this / length
    def normal = Vec(-y, x)
    def center(other: Vec) = Vec((other.x + x) / 2, (other.y + y) / 2)
    def spin(phi: Double) = Vec((x * cos(phi)) - (y * sin(phi)), (x * sin(phi)) + (y * cos(phi)))
  }

  def mkRandColor = {
    def randInt = (Random.nextFloat * 255).toInt
    Color.rgb(randInt, randInt, randInt)
  }

  def mkLine(source: Vec, dest: Vec, width: Double, color: Color): Group = {
    val g = new Group()
    val refline = new Line(source.x, source.y, dest.x, dest.y)
    refline.setStroke(color)
    refline.setStrokeWidth(width / 2)
    val line = new Line(source.x, source.y, dest.x, dest.y)
    line.setStroke(color)
    line.setStrokeWidth(width)
    val blur = new GaussianBlur()
    line.setEffect(blur)
    g.getChildren().addAll(refline, line)
    g
  }

}

