package example.io2

import scala.concurrent.Future

sealed trait IO[+F, +T] {
  def map[U](f: T => U): IO[F, U]
  def flatMap[U, F2 >: F](f: T => IO[F2, U]): IO[F2, U]

  def mapError[F2](f: F => F2): IO[F2, T]

  def transform[F2, T2](f: Either[F, T] => Either[F2, T2]): IO[F2, T2]
  def some[F2 >: F, I](f: => F2)(implicit ev: T <:< Option[I]): IO[F2, I]

  def ensure[F2 >: F](p: T => Boolean, fail: => F2): IO[F2, T]
}

object IO {
  def pure[T](t: => T): IO[Nothing, T] = ???
  def fromFuture[T](future: Future[T]): IO[Throwable, T] = ???

  def swallow[T](io: IO[Throwable, T]): IO[Nothing, T] = ???

  def blockAndRun[F, T](io: IO[F, T]): Either[F, T] = ???
  def asyncRun[F, T](io: IO[F, T]): Future[Either[F, T]] = ???

}

object HttpApi {

  // function transfer(from, to, amount) :   Fail | Unit

  // transfer
  // success -> empty -> (200 + data)
  // --------- status code 4xx
  // fail -> invalid source account 441
  // fail -> invalid target account 442
  // fail -> amount validation      443
  // fail -> insufficient funds     444
  // --------- status code 5xx
  // fail -> db connection
  // fail -> db timeout
  // fail -> other exception
  def transfer(from: String, to: String, amount: Int): IO[Fail, Unit] = ???
}

sealed trait Fail {
  val statusCode: Int
}
case class InvalidSource(id: String) extends Fail {
  override val statusCode: Int = 441
}
case class InvalidTarget(id: String) extends Fail {
  override val statusCode: Int = ???
}
case class InsufficientFunds(id: String, value: Int) extends Fail {
  override val statusCode: Int = ???
}
case class InvalidAmount(amount: Int) extends Fail {
  override val statusCode: Int = ???
}

object Logic {

  def transfer(from: String, to: String, amount: Int): IO[Fail, Unit] = {
    for {
      source <- Data.get(from).some(InvalidSource(from))
      target <- Data.get(to).some(InvalidTarget(to))
      _ <- IO.pure(amount).ensure(_ > 0, InvalidAmount(amount))
      _ <- Data.changeCredit(from, -1 * amount)
      _ <- Data.changeCredit(to, amount).transform(???)
    } yield ()
  }

}

object Data {
  def get(id: String): IO[Nothing, Option[Account]] =
    IO.swallow(IO.fromFuture(DB.get(id)))

  def changeCredit(
      id: String,
      value: Int
  ): IO[Nothing, Unit] = {
    IO.swallow(IO.fromFuture(DB.changeCredit(id, value)).transform {
      case Right(WriteResult(true, _)) => Right(())
      case Right(WriteResult(false, Some(s))) =>
        Left(new Exception(s"Error in db operation: $s on $id and $value"))
      case Right(WriteResult(false, None)) =>
        Left(
          new Exception(
            (s"Error in db operation without description on $id and $value")
          )
        )
      case Left(a) => Left(a)
    })
  }
}
