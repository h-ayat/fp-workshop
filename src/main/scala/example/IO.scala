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

  def map[F, T, U](io: IO[F, T])(f: T => U): IO[F, U] = {
    callback: Callback[F, U] =>
      io(e => callback(e.map(f)))
  }

  val s = simple(12)
  val s2 = map(s)(_ + 2)

  def flatMap[F, T, F2 >: F, U](io: IO[F, T])(f: T => IO[F2, U]): IO[F2, U] = {
    callback: Callback[F2, U] =>
      io(_.fold(f => callback(Left(f)), t => f(t)(callback)))
  }

}

object Test {
  def heavy(list: List[Int]): Double = ???
  def writeToDB(d: Double): Future[Boolean] = ???
  import scala.concurrent.ExecutionContext.Implicits.global

  val r = for {
    v <- IO.simple(heavy(???))
    result <- IO.fromFuture(writeToDB(v))
  } yield {
    result
  }

}
