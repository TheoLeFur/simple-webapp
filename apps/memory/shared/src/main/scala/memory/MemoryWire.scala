package memory

import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException
import cs214.webapp.exceptions.AppException

object MemoryWire extends AppWire[MemoryEvent, MemoryView]:
  import MemoryEvent.*
  import MemoryView.*
  import ujson.*

  override object eventFormat extends WireFormat[MemoryEvent]:
    override def encode(event: MemoryEvent): Value =
      event match
        case Toggle(cardId) => ujson.Arr(ujson.Str("Toggle"), ujson.Num(cardId))
        case FlipSelected   => ujson.Arr(ujson.Str("FlipSelected"))

    override def decode(js: Value): Try[MemoryEvent] =
      Try {
        val arr = js.arr
        val identifier = arr(0).str
        if identifier.equals("Toggle") then Toggle(arr(1).num.toInt)
        else if identifier.equals("FlipSelected") then FlipSelected
        else throw new DecodingException("Invalid identifier")
      }

  override object viewFormat extends WireFormat[MemoryView]:

    override def encode(v: MemoryView): Value =
      v match
        case MemoryView(stateView, alreadyMatched) =>
          ujson.Arr(
            ujson.Str("MemoryView"),
            encodeStateView(stateView),
            encodeAlreadyMatched(alreadyMatched)
          )

    def encodeStateView(stateView: StateView): Value =
      ujson.Arr(
        ujson.Str("StateView"),
        stateView match
          case StateView.Playing(phase, currentPlayer, board) =>
            encodePlaying(phase, currentPlayer, board)
          case StateView.Finished(userIdsSet) => encodeFinished(userIdsSet)
      )
    def decodeStateView(js: Value): StateView =
      val arr = js.arr
      val id = arr(0).str
      if id.equals("StateView") then
        val stateViewArr = arr(1).arr
        val stateViewId = stateViewArr(0).str
        if stateViewId.equals("Playing") then decodePlaying(stateViewArr)
        else if stateViewId.equals("userIdSet") then decodeFinished(stateViewArr)
        else throw new DecodingException(f"Invalid Identifier HERE ${stateViewId}")
      else throw new DecodingException(f"Invalid Identifiegr ${id}")

    def encodePlaying(
        phase: PhaseView,
        currentPlayer: UserId,
        board: Seq[CardView]
    ): Value =
      ujson.Arr(
        ujson.Str("Playing"),
        ujson.Arr(
          ujson.Str("Phase"),
          ujson.Arr(
            phase match
              case PhaseView.SelectingCards => ujson.Str("SelectingCards")
              case PhaseView.CardsSelected  => ujson.Str("CardsSelected")
              case PhaseView.Waiting        => ujson.Str("Waiting")
              case PhaseView.GoodMatch      => ujson.Str("GoodMatch")
              case PhaseView.BadMatch       => ujson.Str("BadMatch")
          )
        ),
        ujson.Arr(
          ujson.Str("CurrentPlayer"),
          ujson.Str(currentPlayer)
        ),
        ujson.Arr(
          ujson.Str("Board"),
          board.map(cardView =>
            cardView match
              case CardView.FaceDown => ujson.Arr(ujson.Str("FaceDown"))
              case CardView.Selected => ujson.Arr(ujson.Str("Selected"))
              case CardView.FaceUp(card) =>
                ujson.Arr(ujson.Str("FaceUp"), ujson.Str(card))
              case CardView.AlreadyMatched(card) =>
                ujson.Arr(ujson.Str("AlreadyMatched"), ujson.Str(card))
          )
        )
      )

    def decodePlaying(js: Value): StateView.Playing =
      val arr = js.arr
      val id = arr(0).str
      if id.equals("Playing") then
        val phaseArr = arr(1).arr
        val currentPlayerArr = arr(2).arr
        val boardArr = arr(3).arr

        val phase: PhaseView = {
          if phaseArr(0).str.equals("Phase") then
            val phaseType = phaseArr(1)(0).str

            if phaseType.equals("SelectingCards") then PhaseView.SelectingCards
            else if phaseType.equals("CardsSelected") then
              PhaseView.CardsSelected
            else if phaseType.equals("Waiting") then PhaseView.Waiting
            else if phaseType.equals("GoodMatch") then PhaseView.GoodMatch
            else if phaseType.equals("BadMatch") then PhaseView.BadMatch
            else throw new DecodingException(f"Invalid Identifier ${phaseType}")
          else
            throw new DecodingException(
              f"Invalid Identifier ${phaseArr(0).str}"
            )
        }

        val currentPlayer: UserId = {
          val currentPlayerArrId = currentPlayerArr(0).str
          if currentPlayerArrId.equals("CurrentPlayer") then
            currentPlayerArr(1).str
          else
            throw new DecodingException(
              f"Invalid Identifier ${currentPlayerArrId}"
            )
        }

        val board: Seq[CardView] = {
          val boardArrId = boardArr(0).str
          if boardArrId.equals("Board") then
            boardArr(1).arr
              .map(elem =>
                val elemId = elem.arr(0).str
                if elemId.equals("FaceDown") then CardView.FaceDown
                else if elemId.equals("Selected") then CardView.Selected
                else if elemId.equals("FaceUp") then
                  CardView.FaceUp(elem.arr(1).str)
                else if elemId.equals("AlreadyMatched") then
                  CardView.AlreadyMatched(elem.arr(1).str)
                else
                  throw new DecodingException(f"Invalid Identifier ${elemId}")
              )
              .toSeq
          else throw new DecodingException(f"Invalid Identifier ${boardArrId}")
        }

        StateView.Playing(phase, currentPlayer, board)
      else throw new DecodingException(f"Invalid Identifier ${id}")

    def encodeFinished(userIdSet: Set[UserId]): Value =
      ujson.Arr(
        ujson.Str("userIdSet"),
        userIdSet.map(id => ujson.Str(id))
      )

    def decodeFinished(js: Value): StateView.Finished =
      val arr = js.arr
      val id = arr(0).str
      if id.equals("userIdSet")
      then StateView.Finished(arr(1).arr.map(id => id.str).toSet)
      else throw new DecodingException(f"Invalid Identifier ${id}")

    def encodeAlreadyMatched(alreadyMatched: Map[UserId, Seq[Card]]): Value =
      ujson.Arr(
        "AlreadyMatched",
        alreadyMatched.toList.map((uid, cards) =>
          ujson.Arr(
            ujson.Str(uid),
            ujson.Arr(cards.map(card => ujson.Str(card))*)
          )
        )
      )

    def decodeAlreadyMatched(js: Value): Map[UserId, Seq[Card]] =
      val arr = js.arr
      val id = arr(0).str
      if id.equals("AlreadyMatched") then
        val subArr = arr(1).arr
        val map = subArr.toList
          .map(kv => {
            val userId: String = kv(0).str
            val sequence: Seq[Card] = kv(1).arr.toSeq.map(card => card.str)
            (userId, sequence)
          })
          .toMap
        map
      else throw new DecodingException(f"Invalid Identifier ${id}")

    override def decode(js: Value): Try[MemoryView] =
      Try {
        val arr = js.arr
        val id = arr(0).str
        if id.equals("MemoryView") then
          MemoryView(
            decodeStateView(arr(1)),
            decodeAlreadyMatched(arr(2))
          )
        else
          println("ERROR")
          throw new DecodingException(f"Invalid identifier ${id}")
      }
