package snake

import cats.data.State
import cats.data.State._
import cats.effect.IO

object GameLogic {
  private def isGameOver: State[GameState, Unit] = for {
    data <- get[GameState]
    _ <- if (data.emptyPositions.isEmpty)
      set(data.copy(isGameOver = true))
    else
      set(data)
  } yield ()

  def gameLogic(dt: Double): State[GameState, IO[Unit]] = for {
    data <- get[GameState]
    _ <- Snake.update(dt)
    isFruitEaten <- Fruit.update
    _ <- if (isFruitEaten) Snake.grow(data.snake) else get[GameState]
    _ <- isGameOver
  } yield IO()
}
