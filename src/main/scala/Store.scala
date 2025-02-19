import cats.data.*
import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.{Event => _, *}
import org.http4s.Uri
import snow.*

case class Store(
    input: SignallingRef[IO, String],
    result: SignallingRef[IO, Result]
)

object Store {
  def apply(window: Window[IO]): Resource[IO, Store] = {
    val key = "nak-input"

    for {
      // if the browser has been loaded with a url like /#/npub1g4f...
      // then we will start our textarea off with that value
      urlparam <- window.location.hash.get.map(_.stripPrefix("#/")).map(Uri.decode(_)).toResource
      input <- SignallingRef[IO].of(urlparam).toResource
      result <- SignallingRef[IO, Result](Left("")).toResource

      _ <- Resource.eval {
        if( urlparam.isEmpty )
          OptionT(window.localStorage.getItem(key))
            .foreachF(input.set(_))
        else
          IO.unit
      }
      
      _ <- window.localStorage
        .events(window)
        .foreach {
          case Storage.Event.Updated(`key`, _, value, _) =>
            input.set(value)
          case _ => IO.unit
        }
        .compile
        .drain
        .background

      _ <- input.discrete
        .evalTap(input => IO.cede *> window.localStorage.setItem(key, input))
        .map(input => (input, Parser.parseInput(input.trim())))
        .evalTap((input, parsed) => result.set(parsed) *> window.location.assign("#/" ++ input.trim()))
        .compile
        .drain
        .background
    } yield Store(input, result)
  }
    
}
