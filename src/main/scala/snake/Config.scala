package snake

import com.googlecode.lanterna.terminal.Terminal

object Config {
  val levelHeight = 10
  val levelWidth = 20

  def getConfig(term: Terminal): Config = {
    val height = term.getTerminalSize.getRows
    val width = term.getTerminalSize.getColumns
    Config(
      width = width,
      height = height,
      x0 = width / 2 - levelWidth / 2,
      y0 = height / 2 - levelHeight / 2,
    )
  }
}

case class Config(width: Int, height: Int, x0: Int, y0: Int)
