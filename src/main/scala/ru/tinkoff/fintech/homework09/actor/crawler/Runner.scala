package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{ActorRef, ActorSystem, Props}

import scala.concurrent.{Future, Promise}

object Runner extends App {
  val http: Http = new Http {
    val internet = Map[Url,Body](
      url("host0", "path0") -> List(url("host0", "path0"), url("host0", "path1"), url("host1", "path0")),
      url("host1", "path0") -> List(url("host1", "path1"), url("host2", "path0"), url("host3", "path0"))
    )

    override def get(url: Url): Future[Body] = Future.successful(
      internet.getOrElse(url, List.empty)
    )
  }

  val parser: Parsr = new Parsr {
    override def links(page: Body): List[Url] = page
  }

  def wrkFactory(manager: ActorRef): Props = Props(new Worker(http, parser, manager))

  val system = ActorSystem("myactorz")

  val startUrl = url("host0", "path0")
  val result = Promise[Map[Host, Int]]()
  system.actorOf(Props(new Manager(wrkFactory, result))) ! Start(startUrl)

  import system.dispatcher
  result.future.foreach{x =>
    println("host stats is: ")
    x.foreach {case (host, count) =>
      println(s"$host \t $count")
    }
  }
}