package coder.simon.scalaz
import scalaz._
import Scalaz._
import OptionT._

object MT2 {

  case class Song(name: String, durationInSeconds: Int)
  case class Album(name: String, songs: List[Song])
  case class Artist(name: String, albums: List[Album])

  def findSong(artists: List[Artist], artistName: String, albumName: String, songName: String): Option[Song] = for {
    artist <- artists find (_.name === artistName)
    album <- artist.albums find (_.name === albumName)
    song <- album.songs find (_.name === songName)
  } yield song

  type IntState[A] = State[Int, A]

  val m1 = "hello".point[IntState]
  val m2 = m1.liftM[OptionT]

  val n1 = "goodbye".some
  val n2 = n1.point[IntState]
  val n3 = optionT(n2)

  def findSong2(artists: List[Artist], artistName: String, albumName: String, songName: String): OptionT[IntState, Song] = for {
    artist <- optionT((artists find (_.name === artistName)).point[IntState])
    album <- optionT((artist.albums find (_.name === albumName)).point[IntState])
    song <- optionT((album.songs find (_.name === songName)).point[IntState])
    _ <- modify { count: Int => if (song.durationInSeconds >= 10 * 60) count + 1 else count }.liftM[OptionT]
  } yield song

  def main(args: Array[String]) = {
    val artists = List.empty[Artist]

    val r1 = findSong2(artists, "Yes", "Going for the One", "Awaken")
    val (x, y) = r1.run.run(0)

    println(x, y)
  }
}