package object example {

  type Callback[-F, -T] = Either[F, T] => Unit
  type IO[F, T] = Callback[F, T] => Unit

  implicit class IOHelper[F, T](io: IO[F, T]) {
    def map[U](f: T => U): IO[F, U] = IO.map(io)(f)

    def flatMap[F2 >: F, U](f: T => IO[F2, U]): IO[F2, U] =
      IO.flatMap[F, T, F2, U](io)(f)
  }
}
