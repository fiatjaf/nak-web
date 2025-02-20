import scala.util.Try
import io.circe.parser.*
import cats.syntax.all.*
import scodec.bits.ByteVector
import scoin.*
import snow.*

type Result = Either[
  String,
  Event | PrivateKey | AddressPointer | EventPointer | ProfilePointer |
    ByteVector32 | Filter
]

object Parser {
  val additions = raw" *\+ *".r

  def parseInput(input: String): Result =
    if input == "" then Left("")
    else
      ByteVector
        .fromHex(input)
        .flatMap(b => Try(Right(ByteVector32(b))).toOption)
        .getOrElse(
          NIP19.decode(input) match {
            case Right(pp: ProfilePointer)             => Right(pp)
            case Right(evp: EventPointer)              => Right(evp)
            case Right(sk: PrivateKey)                 => Right(sk)
            case Right(addr: AddressPointer)           => Right(addr)
            case Left(_) =>
              // parse "a" tag format, nip 33
              val spl = input.split(":")
              val maybeNip33 = 
                if(spl.size == 3) then
                  (
                    spl(0).toIntOption,
                    ByteVector.fromHex(spl(1)),
                    Some(spl(2))
                  ).mapN((kind, author, identifier) =>
                    AddressPointer(
                      identifier,
                      kind,
                      scoin.XOnlyPublicKey(ByteVector32(author)),
                      relays = List.empty
                    )
                  ).toRight("couldn't parse as a nip33 'a' tag")
                else
                  Left("not a nip33, but it might be a filter") // never accessed
              maybeNip33 match {
                case Right(addressPointer) => Right(addressPointer)
                case Left(_) =>
                  // parse event json
                  parse(input) match {
                    case Left(err: io.circe.ParsingFailure) =>
                      Left("not valid JSON or NIP-19 code")
                    case Right(json) =>
                      json
                        .as[Event] match {
                          case Right(event) => Right(event)
                          case Left(err) => decode[Filter](input) match {
                            case Right(filter) => Right(filter)
                            // it is not a filter, so let us now return the error
                            // when we were trying to parse it as an event
                            case Left(_) => err.pathToRootString match {
                              case None       => Left(s"decoding ${err.pathToRootString}")
                              case Some(path) => Left(s"field $path is missing or wrong")
                            }
                          }
                        }
                  }
              }
          }
        )
}
