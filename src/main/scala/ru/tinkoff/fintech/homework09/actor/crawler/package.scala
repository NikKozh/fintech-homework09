package ru.tinkoff.fintech.homework09.actor
import ru.tinkoff.fintech.homework09.crawler.{HttpClient, Parser, Url => AUrl}

import scala.concurrent.Future
import scala.util.Try

package object crawler {
  type Host = String
  type Path = String
  type Url = AUrl[Host,Path]
  type Body = List[Url]

  type Http = HttpClient[Future,Host,Path,Body]
  type Parsr = Parser[Body,Host,Path]

  def url(host: Host, path: Path) = new Url(host, path)

  sealed trait ManagerMessage

  /**
    * Start the crawling process for the given URL. Should be sent only once.
    */
  case class Start(url: Url) extends ManagerMessage
  case class CrawlResult(url: Url, links: List[Url]) extends ManagerMessage

  sealed trait WorkerMessage
  case class Crawl(url: Url) extends WorkerMessage
  case class HttpGetResult(url: Url, result: Try[Body]) extends WorkerMessage
}