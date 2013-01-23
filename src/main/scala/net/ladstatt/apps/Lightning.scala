package net.ladstatt.apps

import javafx.application.Application
import javafx.fxml.FXML
import javafx.fxml.FXMLLoader
import javafx.fxml.Initializable
import javafx.fxml.JavaFXBuilderFactory
import javafx.scene.Parent
import javafx.scene.Scene
import javafx.scene.layout.StackPane
import javafx.stage.Stage
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.layout.Pane
import javafx.scene.layout.BorderPane
import javafx.scene.Group
import javafx.scene.shape.Rectangle
import javafx.scene.paint.Color
import twitter4j.conf.ConfigurationBuilder
import twitter4j.TwitterFactory
import scala.collection.JavaConversions._
import javafx.scene.control.Label
import javafx.scene.layout.FlowPane
import javafx.scene.shape.Line
import javafx.scene.effect.Glow
import javafx.scene.effect.Bloom
import javafx.scene.effect.GaussianBlur
import com.sun.javafx.geom.Vec2d
import scala.util.Random

object Lightning {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[JaggedLine], args: _*)
  }

}

case class Tweet(user: String, tweet: String)

object Twitterant {

  def getTweets: List[Tweet] = try {
    // you have to provide your credentials in the twitter4j.properties file
    val twitter = new TwitterFactory().getInstance()
    val user = twitter.verifyCredentials()
    val statuses = twitter.getHomeTimeline().iterator()
    val x = for (status <- statuses) yield Tweet(status.getUser().getScreenName(), status.getText())
    x.toList
  } catch {
    case te: Throwable => {
      te.printStackTrace()
      println("Failed to get timeline: " + te.getMessage())
      List()
    }
  }

  def mockTweets = List(Tweet("a user", "says some intelligent stuff"))
}

class JaggedLine extends javafx.application.Application {

  val canvasWidth = 1024
  val canvasHeight = 768
  
  val jaggingSections = 100
  val jaggedFactor = 2

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Level1")
    val root = new BorderPane()
    val drawingBoard = new Group()
    val background = {
      val b = new Rectangle(0, 0, canvasWidth, canvasHeight)
      b.setFill(Color.BLACK)
      b
    }
    drawingBoard.getChildren().add(background)
    drawingBoard.getChildren.addAll(jaggedlines(Vec(canvasWidth / 10, canvasHeight / 2), Vec(canvasWidth * 9 / 10, canvasHeight / 2), jaggingSections, Color.WHITESMOKE))
    root.setCenter(drawingBoard)
    primaryStage.setScene(new Scene(root, canvasWidth, canvasHeight))
    primaryStage.show()
  }

  def jaggedlines(source: Vec, dest: Vec, count: Int, color: Color): List[Group] = {

    val totalVec = (source - dest)
    val maxLen = totalVec.length
    val onedir = totalVec.onedir
    val length = totalVec.length / count
    val normal = onedir.normal
    val elongation = jaggedFactor * maxLen / count

    val positions = List(source) ++ (for (i <- 1 to (count - 1)) yield source + onedir * length * i + normal * elongation * Random.nextDouble) ++ List(dest)

    (for (List(a, b) <- positions.sliding(2)) yield mkLine(a, b, color)).toList

  }

  // poor mans helper classes and functions
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

class Lightning extends javafx.application.Application {

  val canvasWidth = 1024
  val canvasHeight = 768

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Lightning strikes")
    val root = new BorderPane()
    val drawingBoard = new Group()
    val background = {
      val b = new Rectangle(0, 0, canvasWidth, canvasHeight)
      b.setFill(Color.BLACK)
      b
    }
    val fp = new FlowPane
    fp.getChildren.addAll(
      for {
        Tweet(user, tweet) <- Twitterant.mockTweets
      } yield {
        val l = new Label("%s : %s".format(user, tweet))
        l.setScaleX(2)
        l.setScaleY(2)
        l
      })

    drawingBoard.getChildren().addAll(fp)
    root.setCenter(drawingBoard)
    primaryStage.setScene(new Scene(root, canvasWidth, canvasHeight))
    primaryStage.show()
  }

}


