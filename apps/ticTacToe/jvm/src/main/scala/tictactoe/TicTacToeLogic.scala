package tictactoe

import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import cs214.webapp.messages.Action
import scala.runtime.LazyVals.Waiting

object TicTacToeStateMachine
    extends cs214.webapp.StateMachine[
      TicTacToeEvent,
      TicTacToeState,
      TicTacToeView
    ]:

  val name: String = "tictactoe"
  val wire = TicTacToeWire
  val numRows: Int = 3
  val numColumns: Int = 3

  override def init(clients: Seq[UserId]): TicTacToeState =
    // Init the game with an empty board. 
    TicTacToeState.InGame(
      Board(Vector.fill[Option[UserId]](numRows, numColumns)(None)),
      clients.last
    )
  // Failures in the Try must hold instances of AppException
  // (from Exceptions.scala under lib/shared/)
  override def transition(
      state: TicTacToeState
  )(uid: UserId, event: TicTacToeEvent): Try[Seq[Action[TicTacToeState]]] =
    state match
      case TicTacToeState.InGame(board, last) =>
        if last == uid then Try { throw new NotYourTurnException() }
        else
          event match
            case TicTacToeEvent.Move(x, y) =>
              if !isInBoard(x, y) then
                Try { throw new IllegalMoveException("Wrong move") }
              else if isBoardFull(board) then
                Try { throw new IllegalMoveException("Board is full") }
              else
                val cell = board.cells(x)(y)
                cell match
                  case Some(value) =>
                    Try { throw new IllegalMoveException("Wrong Move") }
                  case None =>
                    val newBoard = writeToBoard(board, x, y, uid)
                    if isBoardFull(newBoard) then
                      if isWinning(newBoard, uid) then
                        Try {
                          Seq(
                            Action.Render(TicTacToeState.OutofGame(Some(uid)))
                          )
                        }
                      else
                        Try {
                          Seq(Action.Render(TicTacToeState.OutofGame(None)))
                        }
                    else
                      Try {
                        Seq(Action.Render(TicTacToeState.InGame(newBoard, uid)))
                      }
      case TicTacToeState.OutofGame(_) =>
        // One cannot attempt to play when the game is finished
        Try { throw new IllegalMoveException("Board is full") }

  override def project(state: TicTacToeState)(uid: UserId): TicTacToeView =
    state match
      case TicTacToeState.InGame(board, lastUid) =>
        if isWinning(board, uid) then TicTacToeView.Finished(Some(uid))
        else if isWinning(board, lastUid) then
          TicTacToeView.Finished(Some(lastUid))
        else TicTacToeView.Playing(board, lastUid != uid)
      case TicTacToeState.OutofGame(winner) =>
        TicTacToeView.Finished(winner)
  
  /// Returns a new instance of board with new move
  /// @param x : height
  /// @param y : width 
  /// @param uid : user id
  def writeToBoard(board: Board, x: Int, y: Int, uid: UserId): Board =
    val cells = board.cells
    Board(cells.updated(x, cells(x).updated(y, Some(uid))))

  /// Tells us whether the board is currently full 
  /// @param board : board representing the game
  def isBoardFull(board: Board): Boolean =
    board.cells.forall(row =>
      row.forall(cell =>
        cell match
          case Some(value) => true
          case None        => false
      )
    )

  /// Function verifies some index bounds for the move to be in the range of the board
  /// @param x : height
  /// @param y : width

  def isInBoard(x: Int, y: Int): Boolean =
    x >= 0 && x < numRows && y >= 0 && y < numColumns

  /// Function goes through the board and verifies if any of the userIds is winning
  /// @param board : board representing the game
  /// @param userId : userId that is being scanned for

  def isWinning(board: Board, userId: UserId): Boolean =
    val cells: Vector[Vector[Option[UserId]]] = board.cells

    val rowWin =
      cells.exists(row =>
        row.forall(e =>
          e match
            case Some(value) => value == userId
            case None        => false
        )
      )

    val columnWin =
      cells.transpose.exists(row =>
        row.forall(e =>
          e match
            case Some(value) => value == userId
            case None        => false
        )
      )

    val leftDiagonal = (0 until cells.length).map(i => cells(i)(i))
    val rightDiagonal =
      (0 until cells.length).map(i => cells(i)(cells.length - 1 - i))

    val diagonalWin = leftDiagonal.forall(e =>
      e match
        case Some(value) => value == userId
        case None        => false
    )
      ||
        rightDiagonal.forall(e =>
          e match
            case Some(value) => value == userId
            case None        => false
        )

    rowWin || columnWin || diagonalWin

// Server registration magic
class register:
  WebServer.register(TicTacToeStateMachine)
