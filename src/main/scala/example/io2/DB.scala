package example.io2

import scala.concurrent.Future

object DB {
  def get(id: String): Future[Option[Account]] = ???

  def changeCredit(id: String, value: Int): Future[WriteResult] = ???
}

final case class WriteResult(ok: Boolean, message: Option[String])

final case class Account(id: String, credit: Int)
