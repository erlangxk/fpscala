package coder.simon.scalaz.monad

import scalaz.{ Free, ~>, Id, Coyoneda }
import scalaz.std.list._
import scalaz.syntax.traverse._

object F1 extends App {

  type UserId = Int
  type UserName = String
  type UserPhoto = String

  sealed trait Service[A]

  final case class GetTweets(userId: UserId) extends Service[List[Tweet]]
  final case class GetUserName(userId: UserId) extends Service[UserName]
  final case class GetUserPhoto(userId: UserId) extends Service[UserPhoto]

  sealed trait Request[A]
  final case class Pure[A](a: A) extends Request[A]
  final case class Fetch[A](service: Service[A]) extends Request[A]
  type Requestable[A] = Coyoneda[Request, A]

  object Request {
    def pure[A](a: A): Free[Requestable, A] = Free.liftFC(Pure(a): Request[A])
    def fetch[A](service: Service[A]): Free[Requestable, A] = Free.liftFC(Fetch(service): Request[A])
  }

  final case class Tweet(userId: UserId, msg: String)
  final case class User(id: UserId, name: UserName, photo: UserPhoto)

  object ToyInterpreter extends (Request ~> Id.Id) {
    import Id._

    def apply[A](in: Request[A]): Id[A] = in match {
      case Pure(a) => a
      case Fetch(service) => service match {
        case GetTweets(userId) =>
          println(s"Getting tweets for user $userId")
          List(Tweet(1, "Hi"), Tweet(2, "Hi"), Tweet(3, "Bye"))

        case GetUserName(userId) =>
          println(s"Getting user name for user $userId")
          userId match {
            case 1 => "Agnes"
            case 2 => "Brian"
            case _ => "Anonymous"
          }

        case GetUserPhoto(userId) =>
          println(s"Getting user photo for user $userId")
          userId match {
            case 1 => ":-)"
            case 2 => ":-D"
            case _ => ":-|"
          }
      }
    }
  }

  object Example {
    import Request._

    val theId: UserId = 1

    def getUser(id: UserId): Free[Requestable, User] =
      for {
        name <- fetch(GetUserName(id))
        photo <- fetch(GetUserPhoto(id))
      } yield User(id, name, photo)

    val free: Free[Requestable, List[(String, User)]] = for {
      tweets <- fetch(GetTweets(theId))
      result <- tweets.traverseU { t => getUser(t.userId).map(u => (t.msg, u)) }
    } yield result

    def run: List[(String, User)] = Free.runFC(free)(ToyInterpreter)
  }

  val x=Example.run
  println(x)

}