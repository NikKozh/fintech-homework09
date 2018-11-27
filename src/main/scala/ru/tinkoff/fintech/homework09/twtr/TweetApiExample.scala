package ru.tinkoff.fintech.homework09.twtr

import java.time.Instant
import java.util.UUID

import scala.collection.immutable.HashMap
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Future[?]
  *
  *
  * Если же сложилось непоправимое(например обрушилась сеть),
  * то необходимо обработать это достойным образом.
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)


trait TweetStorage {
  def saveTweet(tweet: Tweet): Future[Tweet]
  def loadTweet(id: String): Future[Tweet]
  def updateTweet(newTweet: Tweet): Future[Tweet]
}

final class InMemoryTweetStorage() extends TweetStorage {
  private var storage: Map[String, Tweet] = new HashMap()

  def saveTweet(tweet: Tweet): Future[Tweet] = Future {
    storage.get(tweet.id) match {
      case Some(_) =>
        sys.error(s"saving tweet with id ${tweet.id} already exists in the storage!")
      case None =>
        storage = storage.updated(tweet.id, tweet)
        tweet
    }
  }

  def loadTweet(id: String): Future[Tweet] = Future {
    storage.get(id) match {
      case Some(tweet) => tweet
      case None        => sys.error(s"loading tweet with id $id not found in the storage!")
    }
  }
  
  def updateTweet(newTweet: Tweet): Future[Tweet] = Future {
    storage.get(newTweet.id) match {
      case Some(oldTweet) =>
        if (oldTweet.createdAt != newTweet.createdAt)
          sys.error("updating tweet and old tweet have different data-time creation!")
        else {
          storage = storage.updated(newTweet.id, newTweet)
          storage(newTweet.id)
        }
      case None =>
        sys.error(s"updating tweet with id ${newTweet.id} not found in the storage!")
    }
  }
}

class TweetApi(storage: TweetStorage) {
  def createTweet(request: CreateTweetRequest): Future[Tweet] = request match {
    case CreateTweetRequest(text, _) if text.length > 140 =>
      Future(sys.error("created tweet has more than 140 symbols!"))
    // ...другие проверки, если они понадобятся в будущем...
    case CreateTweetRequest(text, user) => storage.saveTweet {
      Tweet(UUID.randomUUID().toString,
            user,
            text,
            getHashTags(text),
            Some(Instant.now),
            0)
    }
  }

  def getTweet(request: GetTweetRequest): Future[Tweet] =
    storage.loadTweet(request.id)

  def incrementLikes(request: LikeRequest): Future[Int] = {
    for {
      tweet <- storage.loadTweet(request.id)
      newLikes = tweet.likes + 1
      _ <- storage.updateTweet(tweet.copy(likes = newLikes))
    } yield newLikes
  }

  private def getHashTags(text: String): Seq[String] =
    """#[0-9a-zA-Z]+""".r.findAllIn(text).toList.map(hashTag => hashTag.tail)
}

object TweetApiExample extends App {
  val storage: TweetStorage = new InMemoryTweetStorage()
  val app = new TweetApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")
  val result = app.createTweet(request)

  result.onComplete {
    case Success(tweet) => println(s"Created tweet with id: ${tweet.id}")
    case Failure(error) => println(s"Failed to create tweet: $error")
  }

  Await.result(result, 5 seconds)
}