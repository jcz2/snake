package snake

import cats.effect.IO
import Config._
import cats.data.ReaderT
import cats.implicits.catsSyntaxFlatMapOps

object Draw {
  def drawSnake(snake: Snake): ReaderT[IO, Env, Unit] = ReaderT { case Env(config, term) =>
    IO {
      for (Position(x, y) <- snake.positions.init) {
        term.setCursorPosition(config.x0 + x + 1, config.y0 + y + 1)
        term.putCharacter('*')
      }
      val head = Snake.head(snake)
      term.setCursorPosition(config.x0 + head.x + 1, config.y0 + head.y + 1)
      if (snake.isDead) {
        term.putCharacter('X')
      } else {
        term.putCharacter('@')
      }
    }
  }

  def clearPreviousSnake(prevSnake: Snake, currentSnake: Snake): ReaderT[IO, Env, Unit] = ReaderT {
    case Env(config, term) =>
      IO {
        if (Snake.tail(prevSnake) != Snake.tail(currentSnake)) {
          val tail = Snake.tail(prevSnake)
          term.setCursorPosition(config.x0 + tail.x + 1, config.y0 + tail.y + 1)
          term.putCharacter(' ')
        }
      }
  }

  def draw(prevState: GameState, currentState: GameState): ReaderT[IO, Env, Unit] = for {
    _ <- drawBorder()
    _ <- clearPreviousSnake(prevState.snake, currentState.snake)
    _ <- drawSnake(currentState.snake)
    _ <- drawScore(currentState.score)
    _ <- clearPreviousFruit(prevState.fruit, currentState.fruit)
    _ <- drawFruit(currentState.fruit)
    _ <- if (currentState.snake.isDead) drawInstructions else ReaderT.liftF(IO())
  } yield ()

  def drawVerticalLine(x: Int, y: Int, length: Int): ReaderT[IO, Env, Unit] = ReaderT {
    case Env(_, term) =>
      IO {
        for (i <- 0 until length) {
          term.setCursorPosition(x, y + i)
          term.putCharacter('#')
        }
      }
  }

  def drawHorizontalLine(x: Int, y: Int, length: Int): ReaderT[IO, Env, Unit] = ReaderT {
    case Env(_, term) =>
      IO {
        term.setCursorPosition(x, y)
        term.putString("#".repeat(length))
      }
  }

  def drawBorder(): ReaderT[IO, Env, Unit] = ReaderT { case env @ Env(config, _) =>
    (drawHorizontalLine(config.x0, config.y0, levelWidth + 2) >>
      drawVerticalLine(config.x0 + levelWidth + 1, config.y0 + 1, levelHeight) >>
      drawVerticalLine(config.x0, config.y0 + 1, levelHeight) >>
      drawHorizontalLine(config.x0, config.y0 + levelHeight + 1, levelWidth + 2))
      .run(env)
  }

  def drawInstructions: ReaderT[IO, Env, Unit] = ReaderT { case Env(config, term) =>
    IO {
      val text = "Press Enter to restart"
      term.setCursorPosition(config.width / 2 - text.length / 2, config.y0 + levelHeight + 3)
      term.putString(text)
    }
  }

  def drawScore(score: Int): ReaderT[IO, Env, Unit] = ReaderT { case Env(config, term) =>
    IO {
      term.setCursorPosition(config.x0, config.y0 - 2)
      term.putString(s"Score: ${score}")
    }
  }

  def clearPreviousFruit(prevFruit: Fruit, currentFruit: Fruit): ReaderT[IO, Env, Unit] = ReaderT {
    case Env(config, term) =>
      IO {
        if (prevFruit.position != currentFruit.position) {
          term.setCursorPosition(config.x0 + prevFruit.position.x + 1, config.y0 + prevFruit.position.y + 1)
          term.putString(" ")
        }
      }
  }

  def drawFruit(fruit: Fruit): ReaderT[IO, Env, Unit] = ReaderT {
    case Env(config, term) =>
      IO {
        term.setCursorPosition(config.x0 + fruit.position.x + 1, config.y0 + fruit.position.y + 1)
        term.putString("$")
      }
  }
}
