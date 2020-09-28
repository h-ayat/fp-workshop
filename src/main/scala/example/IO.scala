package example

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.Success

trait IO[+F, +T] {

  def exec(callback: Either[F, T] => Unit): Unit

  def map[U](f: T => U): IO[F, U]

  def flatMap[F2 >: F, U](f: T => IO[F2, U]): IO[F2, U]
}

object IO {
  def fromFuture[T](f: Future[T])(implicit
      ec: ExecutionContext
  ): IO[Throwable, T] = new FutureIO(f)

  def pure[T](t: => T): IO[Nothing, T] = new Simple(t)
  def fail[F](f: => F): IO[F, Nothing] = new Fail(f)
}

class FutureIO[T](future: => Future[T])(implicit ec: ExecutionContext)
    extends IO[Throwable, T] {
  override def exec(callback: Either[Throwable, T] => Unit): Unit =
    future.onComplete(a => callback(a.toEither))

  override def map[U](f: T => U): IO[Throwable, U] =
    new FutureIO(future.map(f))

  override def flatMap[F2 >: Throwable, U](f: T => IO[F2, U]): IO[F2, U] = {
    ???
  }

}

class Simple[T](t: => T) extends IO[Nothing, T] {
  override def exec(callback: Either[Nothing, T] => Unit): Unit =
    callback(Right(t))

  override def map[U](f: T => U): IO[Nothing, U] = new Simple(f(t))

  override def flatMap[F2, U](f: T => IO[F2, U]): IO[F2, U] = f(t)
}

class Fail[F](f: => F) extends IO[F, Nothing] {
  override def exec(callback: Either[F, Nothing] => Unit): Unit =
    callback(Left(f))

  override def map[U](f: Nothing => U): IO[F, U] = this

  override def flatMap[F2 >: F, U](f: Nothing => IO[F2, U]): IO[F2, U] = this
}

object Test {
  def heavy(list: List[Int]): Double = ???
  def writeToDB(d: Double): Future[Boolean] = ???
  import scala.concurrent.ExecutionContext.Implicits.global

  val r = for {
    v <- IO.pure(heavy(???))
    result <- IO.fromFuture(writeToDB(v))
  } yield {
    result
  }

}
