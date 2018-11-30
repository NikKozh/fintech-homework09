package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{Actor, ActorLogging, ActorRef}

import scala.collection.mutable
import scala.util.{Failure, Success}

class Worker(http: Http, parser: Parsr, master: ActorRef) extends Actor with ActorLogging {
  implicit val ec = context.dispatcher
  private var isWorking = false
  private val queue = mutable.Queue.empty[Url]

  private def sendToMaster(url: Url, result: List[Url]): Unit = {
    master ! CrawlResult(url, result)
    isWorking = false
    work()
  }

  private def work(): Unit =
    if (!isWorking && queue.nonEmpty) {
      isWorking = true
      val url = queue.dequeue()

      http.get(url).map(b => parser.links(b)).onComplete {
        case Success(urls) => sendToMaster(url, urls)
        case Failure(_)    => sendToMaster(url, Nil)
      }
    }

  override def receive: Receive = {
    case Crawl(url) =>
      queue.enqueue(url)
      work()
  }
}