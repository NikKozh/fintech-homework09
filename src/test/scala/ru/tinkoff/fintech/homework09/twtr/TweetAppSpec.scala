package ru.tinkoff.fintech.homework09.twtr

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class TweetAppSpec extends FlatSpec with Matchers with ScalaFutures {
  val storage = new InMemoryTweetStorage()
  val app = new TweetApi(storage)

  // lazy для того, чтобы проверить метод createTweet в соответствующем тесте, а не прямо здесь
  lazy val createdTweet: Future[Tweet] = app.createTweet(CreateTweetRequest("Text of test tweet", "TestUser"))
  
  behavior of "createTweet method"
  
  it should "create tweet, save it to storage and return if length of tweet text <= 140" in {
    // Первый вызов lazy createdTweet, сначала происходит создание твита
    ScalaFutures.whenReady(createdTweet) { tweet =>
      tweet.text should be("Text of test tweet")
      tweet.user should be("TestUser")
    }
  }

  it should "return failure Future with well described RuntimeException if length of tweet text > 140" in {
    val tooLongTweet =
      app.createTweet(CreateTweetRequest("Tweet with too long text, containing actually more than 140 symbols. " +
                                         "This tweet is invalid and must not be added to storage. " +
                                         "TweetAPI must return Failure with exception on this tweet. " +
                                         "#toolongtweet#nevergowithtoolongtweets", "troll"))

    ScalaFutures.whenReady(tooLongTweet.failed) { exception =>
      exception shouldBe a [RuntimeException]
      exception.getMessage should be ("created tweet has more than 140 symbols!")
    }
  }

  "getTweet method" should "return exactly the same tweet, which was created with same id" in {
    val gotTweet = createdTweet.flatMap(tweet => app.getTweet(GetTweetRequest(tweet.id)))

    ScalaFutures.whenReady(gotTweet zip createdTweet) {
      case (gotOne, createdOne) => gotOne should be(createdOne)
    }
  }

  "incrementLikes method" should "increment tweet likes for 1 and return count of new likes" in {
    val oneLike = createdTweet.flatMap(tweet => app.incrementLikes(LikeRequest(tweet.id)))

    ScalaFutures.whenReady(oneLike) { singleLike =>
      singleLike should be(1)

      val fiveLikes = createdTweet.flatMap { tweet =>
        (1 to 4).foldLeft(Future(0)) { (prevFuture, _) =>
          prevFuture.flatMap(_ => app.incrementLikes(LikeRequest(tweet.id)))
        }
      }

      ScalaFutures.whenReady(fiveLikes) { likesCount =>
        likesCount should be(5)
      }
    }
  }

  "getTweet and incrementLikes methods" should
    "return Failure with well described RuntimeException if tweet with provided id not found" in {

    val nonExistentTweet      = app.getTweet(GetTweetRequest("-1"))
    val failedLikesIncrement  = app.incrementLikes(LikeRequest("-1"))
    val nonexistentTweetError = "loading tweet with id -1 not found in the storage!"

    ScalaFutures.whenReady(nonExistentTweet.failed zip failedLikesIncrement.failed) {
      case (nonExistent, failedLikes) =>
        nonExistent shouldBe a [RuntimeException]
        nonExistent.getMessage should be(nonexistentTweetError)

        failedLikes shouldBe a [RuntimeException]
        failedLikes.getMessage should be(nonexistentTweetError)
    }
  }

  object HashTagExamples {
    val simpleText             = "Simple text without numbers and one hashtag tags\n#singlehashtag"
    val textWithNumbers        = "More complex text with numbers 123 and 2 hash tags\n#firsthashtag#2ndhashtag"
    val textWithMiddleHashTags = "Text with hash tags #rightinthemiddle of text #andwithanotherone\n with line ending"
    val textWithoutHashTags    = "Tweet without hash tags at all"
    val textWithEmptyHashTag   = "Tweet with empty hash tag by user mistake #"

    val textList = List(simpleText, textWithNumbers, textWithMiddleHashTags, textWithoutHashTags, textWithEmptyHashTag)
  }

  "getHashTags method" should "return correct Seq of hash tags from text with hash tags" in {
    val futureTweetList = Future.sequence {
      HashTagExamples.textList.map { text =>
        val request = CreateTweetRequest(text, "usr")
        app.createTweet(request)
      }
    }

    val resultList = List(List("singlehashtag"),
                          List("firsthashtag", "2ndhashtag"),
                          List("rightinthemiddle", "andwithanotherone"),
                          Nil,
                          Nil)

    ScalaFutures.whenReady(futureTweetList) { tweetList =>
      tweetList.zip(resultList).foreach { case (tweet, testHashTags) =>
        tweet.hashTags should be(testHashTags)
      }
    }
  }
}
