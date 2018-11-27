package ru.tinkoff.fintech.homework09.twtr

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.ScalaFutures

class TweetStorageSpec extends FlatSpec with Matchers with ScalaFutures {
  val testStorage: TweetStorage = new InMemoryTweetStorage()
  val testTweet: Tweet = Tweet("1", "user1",
                               "Simple text without numbers and one hashtag tags\n#singlehashtag",
                               List("singlehashtag"), Some(Instant.now), 0)

  behavior of "saveTweet"

  it should "put provided tweet in the Map and return saved instance in success Future " +
            "if tweet with same id was not found" in {
    val savedTweet = testStorage.saveTweet(testTweet)

    ScalaFutures.whenReady(savedTweet) { tweet =>
      tweet should be (testTweet)
    }
  }

  it should "return failure Future with well described RuntimeException " +
            "if provided tweet has the same id as one of existing" in {
    val savedTweet = testStorage.saveTweet(testTweet)

    ScalaFutures.whenReady(savedTweet.failed) { exception =>
      exception shouldBe a [RuntimeException]
      exception.getMessage should be ("saving tweet with id 1 already exists in the storage!")
    }
  }

  behavior of "loadTweet"

  it should "get tweet with provided id from the Map and return its instance in success Future if such was found" in {
    val gottenTweet = testStorage.loadTweet("1")

    ScalaFutures.whenReady(gottenTweet) { tweet =>
      tweet should be (testTweet)
    }
  }

  it should "return failure Future with well described RuntimeException " +
            "if provided tweet was not found in the Map" in {
    val gottenTweet = testStorage.loadTweet("2")

    ScalaFutures.whenReady(gottenTweet.failed) { exception =>
      exception shouldBe a [RuntimeException]
      exception.getMessage should be ("loading tweet with id 2 not found in the storage!")
    }
  }

  behavior of "updateTweet"

  it should "change tweet with provided tweet.id in the Map according to other tweet properties " +
            "and return success Future with its changed instance if such tweet was found" in {
    val changedTweet = testTweet.copy(user = "new_nickname", likes = 10)
    val updatedTweet = testStorage.updateTweet(changedTweet)

    ScalaFutures.whenReady(updatedTweet) { tweet =>
      tweet.user should be ("new_nickname")

      // Также загружаем этот твит через отдельный метод, чтобы убедиться, что изменения точно коснулись хранилища
      val gottenTweet = testStorage.loadTweet("1")
      ScalaFutures.whenReady(gottenTweet) { sameTweet =>
        sameTweet.likes should be (10)
      }
    }
  }

  it should "return failure Future with well described RuntimeException " +
            "if tweet with provided id was not found in the Map" in {
    val nonExistingTweet = Tweet("2", "user2", "text", Seq.empty, None, 0)
    val updatedTweet = testStorage.updateTweet(nonExistingTweet)

    ScalaFutures.whenReady(updatedTweet.failed) { exception =>
      exception shouldBe a [RuntimeException]
      exception.getMessage should be ("updating tweet with id 2 not found in the storage!")
    }
  }
}
