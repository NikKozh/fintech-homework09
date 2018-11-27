package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{Actor, ActorRef, Props}

import scala.concurrent.Promise

class Manager(wrkFactory: ActorRef => Props, result: Promise[Map[Host, Int]]) extends Actor {
  private var referenceCount = Map[Host, Int]()
  private var visitedLinks = Set[Url]()
  private var inProgress = Set[Url]()
  private var workers = Map[Host, ActorRef]()

  override def receive: Receive = {
    case Start(start) =>
      crawlUrl(start)

    case CrawlResult(url, links) =>
      inProgress -= url

      links.foreach { link =>
        crawlUrl(link)
        referenceCount = referenceCount.updated(link.host, referenceCount.getOrElse(link.host, 0) + 1)
      }

      if (inProgress.isEmpty) {
        result.success(referenceCount)
        context.stop(self)
      }
  }

  private def crawlUrl(url: Url): Unit = {
    if (!visitedLinks.contains(url)) {
      visitedLinks += url
      inProgress += url
      actorFor(url.host) ! Crawl(url)
    }
  }

  private def actorFor(host: Host): ActorRef = {
    workers.get(host) match {
      case None =>
        val workerActor = context.actorOf(wrkFactory(self))
        workers += host -> workerActor
        workerActor

      case Some(ar) => ar
    }
  }
}