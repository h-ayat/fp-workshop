package object example {

  type Callback[-F, -T] = Either[F, T] => Unit
  type IO[+F, +T] = Callback[F, T] => Unit

  implicit class IOHelper[F, T](io: IO[F, T]) {
    def map[U](f: T => U): IO[F, U] = IO.map(io)(f)

    def flatMap[F2 >: F, U](f: T => IO[F2, U]): IO[F2, U] =
      IO.flatMap[F, T, F2, U](io)(f)

    def mapFault[F2](f: F => F2): IO[F2, T] = IO.mapFault(io)(f)

    def toOption: IO[Nothing, Option[T]] = IO.toOptional(io)

    def some[F2 >: F, E](
        fault: => F2
    )(implicit evidence: IO[F, T] <:< IO[F, Option[E]]): IO[F2, E] =
      IO.some[F, E, F2](io)(fault)
  }
}

trait IO2[F, T] {
  import scala.concurrent.Future

  def unsafeRun(): Either[F, T]
  def toFuture(): Future[Either[F, T]]
}
