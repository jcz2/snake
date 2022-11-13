package snake

import cats.data.ReaderT
import cats.effect.{IO, Ref}
import com.googlecode.lanterna.input.KeyType

object UserInput {
  val direction: Map[KeyType, Direction] = Map[KeyType, Direction](
    KeyType.ArrowUp -> Direction(0, -1),
    KeyType.ArrowDown -> Direction(0, 1),
    KeyType.ArrowLeft -> Direction(-1, 0),
    KeyType.ArrowRight -> Direction(1, 0)
  )

  def getUserAction(key: KeyType): Option[Direction] = direction.get(key)

  def input(ref: Ref[IO, KeyType]): ReaderT[IO, Env, Unit] = ReaderT { env =>
    for {
      keyType <- IO(env.term.readInput().getKeyType)
      _ <- ref.set(keyType)
      _ <- input(ref).run(env)
    } yield ()
  }

  def read(ref: Ref[IO, KeyType]): IO[KeyType] = for {
    key <- ref.get
    _ <- ref.set(KeyType.Unknown)
  } yield key
}
