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

object Lightning {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Lightning], args: _*)
  }

}

class Lightning extends javafx.application.Application {

  val canvasWidth = 800
  val canvasHeight = 600

  val jaggingSections = 80
  val jaggedFactor = 2
  val lightningTime = 400

  def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] { def handle(e: E) = f(e) }
  val mediaResource = new Media(getClass.getResource("/strike.mp3").toString)
  val catUrl = getClass.getResource("/cat.html").toString
  var cnt = 0
  var catCount = 20
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Lightning strikes (with easter egg)")
    val mainGroup = new Group
    val borderPane = new BorderPane()
    val drawingBoard = new Group()
    drawingBoard.getChildren().add({
      val b = new Rectangle(0, 0, canvasWidth, canvasHeight)
      b.setFill(Color.BLACK)
      b.addEventHandler(MouseEvent.MOUSE_CLICKED, mkEventHandler(
        (e: MouseEvent) => {
          cnt = cnt + 1
          if (cnt < catCount) {
            new MediaPlayer(mediaResource).play
            val startVec = Vec(canvasWidth / 2, 100)
            val destVec = Vec(e.getX, e.getY())
            val jaggedLines = jaggedlines(startVec, destVec, jaggingSections, Color.WHITESMOKE)
            val duration = (Random.nextDouble * lightningTime).toInt
            drawingBoard.getChildren.addAll(jaggedLines.map(withFade(_, duration, Random.nextDouble + 0.2)))
          } else {
            val browser = new WebView()
            browser.setPrefHeight(canvasHeight)
            browser.setPrefWidth(canvasWidth)
            val webEngine = browser.getEngine()
            webEngine.load(catUrl)
            mainGroup.getChildren.clear()
            mainGroup.getChildren().add(browser)
          }
        }))
      b
    })

    borderPane.setCenter(drawingBoard)
    mainGroup.getChildren().add(borderPane)
    primaryStage.setScene(new Scene(mainGroup, canvasWidth, canvasHeight))
    primaryStage.show()
  }

  def withFade(group: Group, duration: Int, brightness: Double) = {
    val ft = new FadeTransition(Duration.millis(duration), group)
    ft.setFromValue(brightness / 8)
    ft.setToValue(brightness)
    ft.setCycleCount(4)
    ft.setAutoReverse(true)
    ft.setOnFinished(mkEventHandler((e: ActionEvent) => {
      group.getChildren().clear()
    }))
    ft.play()
    group
  }

  def jaggedlines(source: Vec, dest: Vec, count: Int, color: Color): List[Group] = {

    val totalVec = (source - dest)
    val maxLen = totalVec.length
    val onedir = totalVec.onedir
    val length = totalVec.length / count
    val normal = onedir.normal
    val elongation = jaggedFactor * maxLen / count

    val wiggle = Random.nextInt(5).toDouble
    val positions = List(source) ++ (for (i <- 1 to (count - 1)) yield source + onedir * length * i + normal * wiggle * elongation * Random.nextDouble) ++ List(dest)

    (for (List(a, b) <- positions.sliding(2)) yield mkLine(a, b, color)).toList

  }

  case class Vec(x: Double, y: Double) {
    def -(that: Vec) = Vec(that.x - x, that.y - y)
    def +(that: Vec) = Vec(x + that.x, y + that.y)
    def *(factor: Double) = Vec(factor * x, factor * y)
    def /(l: Double) = if (l != 0) Vec(x / l, y / l) else sys.error("div.0")
    def length = scala.math.sqrt(x * x + y * y)
    def onedir = this / length
    def normal = Vec(-y, x)
  }

  def mkRandColor = {
    def randInt = (Random.nextFloat * 255).toInt
    Color.rgb(randInt, randInt, randInt)
  }

  def mkLine(source: Vec, dest: Vec, color: Color): Group = {
    val g = new Group()
    val refline = new Line(source.x, source.y, dest.x, dest.y)
    refline.setStroke(color)
    val line = new Line(source.x, source.y, dest.x, dest.y)
    line.setStroke(color)
    line.setStrokeWidth(4)
    val bloom = new Bloom(1.0)
    val blur = new GaussianBlur()
    blur.setInput(bloom)
    line.setEffect(blur)
    g.getChildren().addAll(refline, line)
    g
  }

}

