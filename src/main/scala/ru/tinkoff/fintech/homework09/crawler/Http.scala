package ru.tinkoff.fintech.homework09.crawler

sealed case class Url[Host,Path](host: Host, path: Path)

trait HttpClient[F[_],Host,Path,Body] {
  def get(url: Url[Host,Path]): F[Body]
}

trait Parser[Body,Host,Path] {
  def links(page: Body): List[Url[Host,Path]]
}