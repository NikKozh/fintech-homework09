package ru.tinkoff.fintech.homework09.monix.crawler

import monix.eval.{Fiber, Task}
import monix.execution.AsyncQueue
import monix.execution.Scheduler.Implicits.global
import cats.implicits._
import scala.concurrent.duration._

import scala.concurrent.Await
import ru.tinkoff.fintech.homework09.answers.monix.{Worker => AWorker}


object Runner extends App {
  val http: Http = new Http {
    val internet = Map[Url,Body](
      url("host0", "path0") -> List(url("host0", "path0"), url("host0", "path1"), url("host1", "path0")),
      url("host1", "path0") -> List(url("host1", "path1"), url("host2", "path0"), url("host3", "path0"))
    )

    override def get(url: Url): Task[Body] = Task (
      internet.getOrElse(url, List.empty)
    )
  }

  val parser: Parsr = new Parsr {
    override def links(page: Body): List[Url] = page
  }

  println(
    Await.result(new CrawlRoutines(http, parser).crawl(url("host0", "path0")).runToFuture, 10 seconds)
  )
}

class CrawlRoutines(
  val http: Http,
  val parseLinks: Parsr) extends Worker with Manager {

  def crawl(crawlUrl: Url): Task[Map[Host, Int]] = {

    val crawlerQueue = MQueue.make[CrawlerMessage]
    for {
      _ <- crawlerQueue.offer(Start(crawlUrl))
      r <- crawler(crawlerQueue, CrawlerData(Map(), Set(), Set(), Map()))
    } yield r
  }
}