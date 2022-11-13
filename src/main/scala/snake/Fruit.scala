package snake

import cats.data.State
import cats.data.State._

import scala.util.Random

case class Fruit(position: Position)

object Fruit {
  def spawn(emptyPositions: Set[Position]): Fruit = {
    val i = Random.nextInt(emptyPositions.size)
    Fruit(position = emptyPositions.view.slice(i, i + 1).head)
  }

  private def isCollidingWithSnake(snake: Snake, fruit: Fruit): Boolean =
    snake.positions.contains(fruit.position)

  type IsFruitEaten = Boolean

  def update: State[GameState, IsFruitEaten] = for {
    data <- get[GameState]
    isFruitEaten = isCollidingWithSnake(data.snake, data.fruit)
    _ <- if (isFruitEaten) {
      val newFruit = Fruit.spawn(data.emptyPositions)
      set(data.copy(
        emptyPositions = data.emptyPositions + data.fruit.position - newFruit.position,
        fruit = newFruit,
        score = data.score + 1
      ))
    } else {
      set(data)
    }
  } yield isFruitEaten
}
