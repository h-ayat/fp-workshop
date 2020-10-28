package example

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.Success

object IO {

  def simple[T](t: => T): IO[Nothing, T] = pure(Right(t))

  private def pure[F, T](f: => Either[F, T]): IO[F, T] = {
    callback: Callback[F, T] =>
      callback(f)
  }

  def fromFuture[T](f: Future[T])(implicit
      ec: ExecutionContext
  ): IO[Throwable, T] = { callback: Callback[Throwable, T] =>
    f.onComplete { tr =>
      callback(tr.toEither)
    }
  }

  def transform[F, T, F2, T2](io: IO[F, T])(
      f: Either[F, T] => Either[F2, T2]
  ): IO[F2, T2] = { callback: Callback[F2, T2] => io(e => callback(f(e))) }

  def some[F, T, F2 >: F](io: IO[F, Option[T]])(fault: => F2): IO[F2, T] =
    transform(io) {
      case Right(Some(t)) => Right(t)
      case Right(None)    => Left(fault)
      case Left(f)        => Left(f)
    }

  def toOptional[T](io: IO[_, T]): IO[Nothing, Option[T]] =
    transform(io) {
      case Right(t) => Right(Some(t))
      case _        => Right(None)
    }

  def toEither[F, T](io: IO[F, T]): IO[Nothing, Either[F, T]] =
    transform(io)(Right.apply)

  def fold[F, T, F2, T2](io: IO[F, T])(f1: F => F2, f2: T => T2): IO[F2, T2] = {
    transform(io) {
      case Right(t) => Right(f2(t))
      case Left(f)  => Left(f1(f))
    }
  }

  def mapFault[F, T, F2](io: IO[F, T])(fun: F => F2): IO[F2, T] =
    transform(io) {
      case Right(t) => Right(t)
      case Left(f)  => Left(fun(f))
    }

  def map[F, T, U](io: IO[F, T])(f: T => U): IO[F, U] =
    transform(io) {
      case Right(t) => Right(f(t))
      case Left(f)  => Left(f)
    }

  val s = simple(12)
  val s2 = map(s)(_ + 2)

  def flatMap[F, T, F2 >: F, U](io: IO[F, T])(f: T => IO[F2, U]): IO[F2, U] = {
    callback: Callback[F2, U] =>
      io(_.fold(f => callback(Left(f)), t => f(t)(callback)))
  }
}

object TestLogic {
  import scala.concurrent.ExecutionContext.Implicits.global

  case class Person(id: String, name: String)
  case class NotFoundError(id: String)

  private def getUserFromDB(id: String): Future[Person] = ???

  def getUser(id: String): IO[NotFoundError, Person] =
    IO.fromFuture(getUserFromDB(id)).mapFault(_ => NotFoundError(id))

  def getUser2(id: String): IO[Nothing, Option[Person]] =
    IO.transform(IO.fromFuture(getUserFromDB(id))) {
      case Right(t) => Right(Some(t))
      case Left(f)  => Right(None)
    }

  def render(p: Person): String = ???
  def prettify(html: String): String = ???
  def optimize[F](html: String): IO[F, String] = ???

  def render(id: String): IO[NotFoundError, String] =
    for {
      person <- IO.some(getUser2(id))(NotFoundError(id))
      html1 = render(person)
      optimizedHTML <- optimize(html1)
    } yield {
      optimizedHTML
    }
}

object Test {
  def heavy(list: List[Int]): Double = ???
  def writeToDB(d: Double): Future[Boolean] = ???
  import scala.concurrent.ExecutionContext.Implicits.global

  val r: IO[Throwable, Boolean] = for {
    v <- IO.simple(heavy(???))
    result <- IO.fromFuture(writeToDB(v))
  } yield {
    result
  }

  val a: IO[String, Int] = ???
}
