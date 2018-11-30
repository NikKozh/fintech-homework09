package ru.tinkoff.fintech.homework09.monix.crawler
import monix.eval.{Fiber, Task}

trait Worker {
  def http: Http
  def parseLinks: Parsr

  def worker(workerQueue: MQueue[Url], crawlerQueue: MQueue[CrawlerMessage]): Task[Fiber[Unit]] = {
    def doWork(task: Task[Url]): Task[Unit] = {
      task.flatMap { url =>
        http.get(url).map { body =>
          parseLinks.links(body)
        }.onErrorRecover {
          case _: Exception => Nil
        }.flatMap { links =>
          crawlerQueue.offer(CrawlResult(url, links))
        }.flatMap { _ =>
          doWork(workerQueue.take)
        }
      }
    }

    doWork(workerQueue.take).start
  }
}