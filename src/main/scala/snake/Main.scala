package snake

import cats.data.{ReaderT, StateT}
import cats.effect._
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import com.googlecode.lanterna.terminal.ansi.UnixLikeTerminal
import StateT._

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object Main extends IOApp {
  def loop(config: Config, term: Terminal, ref: Ref[IO, KeyType]): StateT[IO, GameState, Unit] = for {
    currentTime <- liftF(Clock[IO].realTime)
    state <- get[IO, GameState]
    dt = (currentTime.length - state.time.length) / 1000d
    userInput <- liftF(UserInput.read(ref))
    userAction = UserInput.getUserAction(userInput)
    _ <- StateT.fromState[IO, GameState, Unit](GameLogic.gameLogic(dt))
    newGameState <- get[IO, GameState].map(state => state.copy(
      time = currentTime,
      direction = userAction
        .filter(dir => Snake.isDirectionAllowed(dir).runA(state).value)
        .getOrElse(state.direction)))
    _ <- liftF(Draw.draw(state, newGameState).run(Env(config, term)))
    _ <- set[IO, GameState](newGameState)
    _ <- liftF(IO().delayBy(FiniteDuration(16, TimeUnit.MILLISECONDS)))
    _ <- if (newGameState.snake.isDead || newGameState.isGameOver) liftF[IO, GameState, Unit](IO()) else loop(config, term, ref)
  } yield ()


  def pressEnterToRestart(ref: Ref[IO, KeyType]): ReaderT[IO, Env, Unit] = for {
    input <- ReaderT.liftF(UserInput.read(ref))
    _ <- if (input == KeyType.Enter) {
      main(ref)
    } else if (input != KeyType.Escape) {
      pressEnterToRestart(ref)
    } else {
      ReaderT.liftF(IO())
    }
  } yield ()

  def main(ref: Ref[IO, KeyType]): ReaderT[IO, Env, Unit] = for {
    _ <- ReaderT[IO, Env, Unit](env => IO(env.term.clearScreen()))
    _ <- ReaderT[IO, Env, FiberIO[Unit]](env => UserInput.input(ref).run(env).start)
    s <- ReaderT[IO, Env, GameState](_ => GameState.init)
    gameState <- ReaderT[IO, Env, GameState](env => loop(env.config, env.term, ref).runS(s))
    _ <- Draw.draw(gameState, gameState)
    _ <- pressEnterToRestart(ref)
  } yield ()

  def createTerminal(): IO[Terminal] = IO {
    val term: Terminal = new DefaultTerminalFactory()
      .setUnixTerminalCtrlCBehaviour(UnixLikeTerminal.CtrlCBehaviour.CTRL_C_KILLS_APPLICATION)
      .createTerminal()
    term
      .setCursorVisible(false)
    term
  }

  override def run(args: List[String]): IO[ExitCode] = for {
    term <- createTerminal()
    config = Config.getConfig(term)
    env = Env(config, term)
    _ <- IO(term.enterPrivateMode())
    ref <- IO.ref[KeyType](KeyType.Unknown)
    _ <- main(ref).run(env)
  } yield ExitCode.Success
}
