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
import javafx.scene.SnapshotParameters
import javafx.scene.image.PixelReader

object Lightning {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Lightning], args: _*)
  }

}

class Lightning extends javafx.application.Application {

  val headline = "FX"
  val appTitle = "JavaFX Lightning"

  // example for loading a true type font
  //val font = Font.loadFont(getClass.getResourceAsStream("/alph.ttf"), 250)

  // this effect works well with some fonts, less with others. 
  val font = Font.font("Verdana", 350)

  val canvasWidth = 800
  val canvasHeight = 600

  val lightningTime = 15400
  val displacement = 50
  val curDetail = 5

  def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] { def handle(e: E) = f(e) }
  var running = false

  def mkFadingBolt(start: Vec, dest: Vec, width: Double, branchCnt: Int, col: Color, group: Group) = {
    val boltParts = mkBolt(mkBranches(start, dest, displacement, branchCnt), width, col)
    val duration = (Random.nextDouble * lightningTime).toInt
    val bolt = new Group
    bolt.getChildren.addAll(boltParts.map(withFade(group, bolt, _, duration, Random.nextDouble + 0.2)))
    group.getChildren.addAll(bolt)
  }

  def mkPointsOnText(offset: Vec, pReader: PixelReader, width: Int, height: Double, chaos: Int) = {

    def mkRandPoint: Vec = {
      val x = ((Random.nextDouble * width)).toInt
      val y = ((Random.nextDouble * height)).toInt
      val b = pReader.getColor(x, y)
      if (b != Color.WHITE) offset + Vec(x, y) else mkRandPoint
    }

    val (src, dest, _) = (for (i <- 0 to chaos) yield {
      val src = mkRandPoint
      val dest = mkRandPoint
      (src, dest, (src - dest).length)
    }).sortWith((a, b) => a._3 < b._3).head

    (src, dest)
  }

  def mkTimeline(group: Group, offset: Vec, pReader: PixelReader, width: Int, height: Int, color: Color) = {
    val timeline = new Timeline
    timeline.setRate(24)
    timeline.setCycleCount(Animation.INDEFINITE)
    var chaos = 1
    timeline.getKeyFrames().add(
      new KeyFrame(Duration.seconds(1),
        new EventHandler[ActionEvent]() {
          def handle(event: ActionEvent) {
            if (Random.nextDouble < 0.2) chaos = if (chaos > 40) 1 else chaos + 1
            val (start, dest) = mkPointsOnText(offset, pReader, width, height, chaos)
            val boltWidth = Random.nextInt(2) + 1
            val branch = Random.nextInt(1)
            mkFadingBolt(start, dest, boltWidth, branch, color, group)
          }
        }))
    timeline.play()
  }

  // calculate pixelraster for a given text and font
  def mkSnapShot(headline: String, font: Font): (PixelReader, Int, Int) = {
    val t = new Text(headline)
    val textColor = Color.WHITESMOKE
    t.setFont(font)
    t.setStroke(textColor.brighter)
    t.setFill(textColor.darker)
    val s = t.snapshot(new SnapshotParameters(), null)
    (s.getPixelReader(), t.getBoundsInParent().getWidth.toInt, t.getBoundsInParent.getHeight.toInt)
  }

  def startFX(drawingBoard: Group, pReader: PixelReader, width: Int, height: Int) = mkEventHandler(
    (e: MouseEvent) => {
      if (!running) {
        mkTimeline(drawingBoard, Vec((canvasWidth - width) / 2, (canvasHeight - height) / 2), pReader, width, height, Color.WHITE)
        running = true
      }
    })

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle(appTitle)
    val (pReader, width, height) = mkSnapShot(headline, font)
    val borderPane = new BorderPane()
    val drawingBoard = new Group()
    drawingBoard.getChildren().add({
      val b = new Rectangle(0, 0, canvasWidth, canvasHeight)
      b.setFill(Color.BLACK)
      b.addEventHandler(MouseEvent.MOUSE_CLICKED, startFX(drawingBoard, pReader, width, height))
      b
    })

    borderPane.setCenter(drawingBoard)
    primaryStage.setScene(new Scene(borderPane, canvasWidth, canvasHeight))
    primaryStage.show()
  }

  def withFade(grandParent: Group, parent: Group, group: Group, duration: Int, brightness: Double) = {
    val ft = new FadeTransition(Duration.millis(duration), group)
    ft.setFromValue(0)
    ft.setToValue(brightness)
    ft.setCycleCount(1)
    ft.setOnFinished(mkEventHandler((e: ActionEvent) => {
      group.getChildren().clear()
      parent.getChildren.remove(group)
      grandParent.getChildren.remove(parent)
    }))
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

  def mkBranches(origSource: Vec, origDest: Vec, displacement: Double, branchCnt: Int): List[(Vec, Vec)] = {

    def mkPoints(source: Vec, dest: Vec, branchCnt: Int): List[(Vec, Vec)] = {
      branchCnt match {
        case 0 => mkMidPointReplacement(source, dest, displacement, curDetail)
        case _ => {
          val listOfVecPairs = mkMidPointReplacement(source, dest, displacement, curDetail)
          val idx = (listOfVecPairs.size / 2).toInt
          val (newStartPos, _) = listOfVecPairs(idx)
          listOfVecPairs ++
            (if (Random.nextInt(3) > 1)
              mkBranches(newStartPos, newStartPos + (dest - newStartPos).spin(3 * Pi / 4), displacement, branchCnt - 1)
            else List()) ++
            (if (Random.nextInt(3) > 1)
              mkBranches(newStartPos, newStartPos + (dest - newStartPos).spin(5 * Pi / 4), displacement, branchCnt - 1)
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

