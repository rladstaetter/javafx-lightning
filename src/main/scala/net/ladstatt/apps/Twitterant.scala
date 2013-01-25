package net.ladstatt.apps

import scala.collection.JavaConversions.asScalaIterator
import scala.collection.JavaConversions.seqAsJavaList

import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.control.Label
import javafx.scene.layout.BorderPane
import javafx.scene.layout.FlowPane
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.stage.Stage
import twitter4j.TwitterFactory

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

class Twitterant extends javafx.application.Application {

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


