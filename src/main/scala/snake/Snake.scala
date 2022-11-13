package snake

import cats.data.State
import cats.data.State._

case class Snake(speed: Int, positions: Vector[Position], isDead: Boolean)

object Snake {
  private def updatePositions(direction: Direction, distance: Int): State[GameState, Vector[Position]] = for {
    data <- get[GameState]
    snake = data.snake
    positions = for (i <- snake.positions.indices) yield {
      snake.positions.lift(i + distance) match {
        case Some(p) => p
        case None =>
          val moves = distance - ((snake.positions.length - i) - 1)
          Position(
            snake.positions.last.x + direction.x * moves,
            snake.positions.last.y + direction.y * moves
          )
      }
    }
  } yield positions.toVector

  private def isCollidingWithSelf: State[GameState, Boolean] = for {
    data <- get[GameState]
    head = Snake.head(data.snake)
  } yield data.snake.positions.init.contains(head)

  private def isCollidingWithWalls: State[GameState, Boolean] = for {
    data <- get[GameState]
    head = Snake.head(data.snake)
  } yield head.x < 0 || head.x >= Config.levelWidth ||
    head.y < 0 || head.y >= Config.levelHeight

  def isDirectionAllowed(direction: Direction): State[GameState, Boolean] = for {
    data <- get[GameState]
    head = Snake.head(data.snake)
    second = data.snake.positions.init.last
  } yield !(head.x + direction.x == second.x && head.y + direction.y == second.y)

  def head(snake: Snake): Position =
    snake.positions.last

  def tail(snake: Snake): Position =
    snake.positions.head

  def grow(prevSnake: Snake): State[GameState, Unit] = for {
    data <- get[GameState]
    snake = data.snake
    _ <- set(data.copy(
      snake = snake.copy(
        positions = snake.positions.prepended(Snake.tail(prevSnake))
      )
    ))
  } yield ()

  def update(dt: Double): State[GameState, Unit] = for {
    data <- get[GameState]
    d = data.distance.toInt
    newDistance = data.distance % 1
    positions <- updatePositions(data.direction, d)
    isCollidingWithSelf <- isCollidingWithSelf
    isCollidingWithWalls <- isCollidingWithWalls
    isDead = isCollidingWithSelf || isCollidingWithWalls
    snake = data.snake.copy(positions = positions, isDead = isDead)
    emptyPositions = data.emptyPositions ++ data.snake.positions -- snake.positions
    _ <- set(data.copy(snake = snake, emptyPositions = emptyPositions, distance = newDistance + dt * data.snake.speed))
  } yield ()
}
