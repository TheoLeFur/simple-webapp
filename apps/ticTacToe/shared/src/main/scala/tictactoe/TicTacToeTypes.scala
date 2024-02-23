package tictactoe

import cs214.webapp.UserId

import scala.util.{Failure, Success, Try}

/** Stores all information about the current game. */
enum TicTacToeState:
  // There are some cases left in the board, make sure the previous player is not the same as the
  // current player
  case InGame(board: Board, lastPlayerUid: UserId)
  // The board is full, declare a winner if there is one
  case OutofGame(winner: Option[UserId])

/** There is only one event in tic-tac-toe: clicking a cell. */
enum TicTacToeEvent:
  /** User clicked cell (x, y) */
  case Move(x: Int, y: Int)

/** Client views reflect the state of the game: playing or finished. */
enum TicTacToeView:
  /** Game in progress. */
  case Playing(board: Board, yourTurn: Boolean)

  /** Game over. [[winner]] is [[None]] if the game ended in a tie. */
  case Finished(winner: Option[UserId])

// Here it is important to use an immutable collection like Vector
case class Board(cells: Vector[Vector[Option[UserId]]]):

  val numRows = 3
  val numColumns = 3

  /** Get the value in the cell at (r, c). */
  def apply(r: Int, c: Int): Option[UserId] =
    require(r >= 0 && c >= 0 && r < numRows && c < numColumns)
    cells(r)(c)
