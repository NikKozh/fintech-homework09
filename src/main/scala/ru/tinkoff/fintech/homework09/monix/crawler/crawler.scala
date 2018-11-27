package ru.tinkoff.fintech.homework09.monix

import monix.eval.{Fiber, Task}
import monix.execution.{AsyncQueue, Scheduler}
import ru.tinkoff.fintech.homework09.crawler.{HttpClient, Parser, Url => AUrl}

package object crawler {
  type Host = String
  type Path = String
  type Url = AUrl[Host,Path]
  type Body = List[Url]

  type Http = HttpClient[Task,Host,Path,Body]
  type Parsr = Parser[Body,Host,Path]

  def url(host: Host, path: Path) = new Url(host, path)


  case class WorkerData(queue: MQueue[Url], fiber: Fiber[Unit])
  case class CrawlerData(referenceCount: Map[Host, Int], visitedLinks: Set[Url], inProgress: Set[Url], workers: Map[Host, WorkerData])

  sealed trait CrawlerMessage

  /**
    * Start the crawling process for the given URL. Should be sent only once.
    */
  case class Start(url: Url) extends CrawlerMessage
  case class CrawlResult(url: Url, links: List[Url]) extends CrawlerMessage

  class MQueue[T](q: AsyncQueue[T]) {
    def take: Task[T] = {
      Task.deferFuture(q.poll())
    }
    def offer(t: T): Task[Unit] = {
      Task.eval(q.offer(t))
    }
  }
  object MQueue {
    def make[T](implicit scheduler: Scheduler): MQueue[T] = new MQueue(AsyncQueue.bounded(16))
  }
}