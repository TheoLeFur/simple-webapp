package tictactoe

import ujson.*
import scala.util.{Failure, Success, Try}

import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException
import cs214.webapp.{AppWire, WireFormat, UserId}

object TicTacToeWire extends AppWire[TicTacToeEvent, TicTacToeView]:
  import TicTacToeEvent.*
  import TicTacToeView.*

  override object eventFormat extends WireFormat[TicTacToeEvent]:
    override def encode(t: TicTacToeEvent): Value =
      t match
        case Move(x, y) =>
          ujson.Arr(
            ujson.Str("Move"),
            ujson.Num(x),
            ujson.Num(y)
          )
    override def decode(json: Value): Try[TicTacToeEvent] =
      Try {
        val arr = json.arr
        if arr(0).str == "Move" then
          val x = arr(1).num.toInt
          val y = arr(2).num.toInt
          Move(x, y)
        else throw DecodingException(f"Unexpected: ${arr.toList}")
      }

  override object viewFormat extends WireFormat[TicTacToeView]:

    def encode(t: TicTacToeView): Value =
      t match
        case Playing(board, yourTurn) =>
          ujson.Arr(
            "Playing",
            ujson.Arr(
              board.cells.map(col =>
                ujson.Arr(
                  col.map(e =>
                    e match
                      case Some(value) => ujson.Arr("Some", ujson.Str(value))
                      case None        => ujson.Arr("None")
                  )*
                )
              )*
            ),
            ujson.Bool(yourTurn)
          )
        case Finished(winner) =>
          ujson.Arr(
            ujson.Str("Finished"),
            winner match
              case Some(value) => ujson.Arr(ujson.Str("Some"), ujson.Str(value))
              case None        => ujson.Arr(ujson.Str("None"))
          )

    def decode(json: Value): Try[TicTacToeView] =
      Try {
        val arr = json.arr
        val identifier = arr(0).str

        if identifier == "Playing" then
          val turn: Boolean = arr(2).bool
          val cells = arr(1).arr
            .map(col => {
              col.arr
                .map(value => {
                  val subArr = value.arr
                  if subArr(0).str == "Some" then Some(subArr(1).str)
                  else if subArr(0).str == "None" then None
                  else
                    throw DecodingException(f"Illegal identifier: ${subArr(0)}")
                })
                .toVector
            })
            .toVector

          Playing(Board(cells), turn)
        else if identifier == "Finished" then
          val subArr = arr(1).arr
          val subArrId = subArr(0).str

          if subArrId == "Some" then Finished(Some(subArr(1).str))
          else if subArrId == "None" then Finished(None)
          else
            throw new DecodingException(
              f"Illegal identifier: ${subArrId}"
            )
        else
          throw new DecodingException(
            f"Illegal identifier: ${identifier}"
          )
      }
