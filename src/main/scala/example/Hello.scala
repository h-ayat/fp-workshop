package example

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Hello extends App {}

trait Fault {
  val code: Int
  val message: String
}

class Result[T](val value: Future[Either[Fault, T]]) {

  def map[U](f: T => U): Result[U] = new Result(value.map(_.map(f)))

  def flatMap[U](f: T => Result[U]): Result[U] =
    new Result(value.flatMap {
      case Right(t) => f(t).value
      case Left(f)  => Future.successful(Left(f))
    })

}

trait Monad[M[_]] {
  def flatMap[T, U](m: M[T])(f: T => M[U]): M[U]
  def pure[T](t: T): M[T]
}

class EitherT[U[_], F, T](value: U[Either[F, T]]) {
  def flatMap[V](f: T => U[Either[F, V]]): U[Either[F, V]] = ???
  def pure(t: T): U[Either[F, T]] = ???
}

class EitherTransformer {
  def apply[U[_], F, T](input: U[Either[F, T]]): EitherT[U, F, T] = ???
}

object Logic {

  def getPerson(id: String): Result[Person] = ???

  def getPersonAndFriend2(id: String): Result[(Person, Person, Person)] = {
    /*getPerson(id).flatMap {
      case Right(person) =>
        getPerson(person.friendId).flatMap {
          case Right(friend) =>
            getPerson(friend.friendId).map {
              _.map(friendOfFriend => (person, friend, friendOfFriend))
            }
          case Left(f) => Future.successful(Left(f))
        }
      case Left(f) => Future.successful(Left(f))
    }*/
    ???
  }

  def getPersonAndFriend(id: String): Result[(Person, Person, Person)] =
    for {
      person <- getPerson(id)
      friend <- getPerson(person.friendId)
      friendOfFriend <- getPerson(friend.friendId)
    } yield (person, friend, friendOfFriend)

}

case class Person(id: String, name: String, friendId: String)
