package snake

import cats.effect.{Clock, IO}

import scala.concurrent.duration.FiniteDuration

case class GameState(
  time: FiniteDuration,
  score: Int,
  direction: Direction,
  snake: Snake,
  distance: Double,
  emptyPositions: Set[Position],
  fruit: Fruit,
  isGameOver: Boolean
)

object GameState {
  private def getEmptyPositions(snake: Snake): Set[Position] = {
    val emptyPositions = for (
      i <- 0 until Config.levelWidth;
      j <- 0 until Config.levelHeight;
      if !snake.positions.contains(Position(i, j))
    ) yield Position(i, j)
    emptyPositions.toSet
  }

  def init: IO[GameState] = for {
    time <- Clock[IO].realTime
  } yield {
    val y = Config.levelHeight / 2
    val snake = Snake(
      speed = 5,
      positions = Vector(
        Position(1, y),
        Position(2, y),
        Position(3, y),
        Position(4, y),
        Position(5, y)
      ),
      isDead = false
    )
    val emptyPositions = getEmptyPositions(snake)
    GameState(
      score = 0,
      snake = snake,
      direction = Direction(1, 0),
      time = time,
      distance = 0,
      emptyPositions = emptyPositions,
      fruit = Fruit.spawn(emptyPositions),
      isGameOver = false
    )
  }
}
