package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{Actor, ActorLogging, ActorRef}

class Worker(http: Http, parser: Parsr, master: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = ???
}
