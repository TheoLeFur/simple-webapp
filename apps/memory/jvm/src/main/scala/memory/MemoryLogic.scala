package memory

import scala.util.{Try, Random}

import ujson.Value

import cs214.webapp.*
import cs214.webapp.messages.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer

import memory.*
import cats.syntax.contravariant

// Feel free to tweak this value!
private val SHOW_CARDS_PAUSE_MS = 2500

object MemoryStateMachine
    extends cs214.webapp.StateMachine[MemoryEvent, MemoryState, MemoryView]:

  val name: String = "memory"
  val wire = MemoryWire

  def Deck(cards: String): Vector[String] =
    cards.trim.split(" +").to(Vector)

  val DECKS: Map[String, Vector[String]] = Map(
    "Simple" -> Deck("""
      💫 ⭐️
    """),
    "Stars" -> Deck("""
      💫 ⭐️ 🌟 ✨ ☀️
    """),
    "Animals" -> Deck("""
      🐵 🐒 🦍 🦧 🐶 🐕 🦮 🐕‍🦺
      🐩 🐺 🦊 🦝 🐱 🐈 🐈‍⬛ 🦁
      🐯 🐅 🐆 🐴 🫎 🫏 🐎 🦄
      🦓 🦌 🦬 🐮 🐂 🐃 🐄 🐷
      🐖 🐗 🐽 🐏 🐑 🐐 🐪 🐫
      🦙 🦒 🐘 🦣 🦏 🦛 🐭 🐁
      🐀 🐹 🐰 🐇 🐿️ 🦫 🦔 🦇
      🐻 🐻‍❄️ 🐨 🐼 🦥 🦦 🦨 🦘
      🦡
    """),
    "Birds" -> Deck("""
      🦃 🐔 🐓 🐣 🐤 🐥 🐦 🐧
      🕊️ 🦅 🦆 🦢 🦉 🦤 🪶 🦩
      🦚 🦜 🪽 🐦‍⬛ 🪿
    """),
    "Marine & Reptiles" -> Deck("""
      🐸 🐊 🐢 🦎 🐍 🐲 🐉 🦕
      🦖 🐳 🐋 🐬 🦭 🐟 🐠 🐡
      🦈 🐙 🐚 🪸 🪼 🦀 🦞 🦐
      🦑 🦪
    """),
    "Bugs" -> Deck("""
      🐌 🦋 🐛 🐜 🐝 🪲 🐞 🦗
      🪳 🕷️ 🕸️ 🦂 🦟 🪰 🪱 🦠
    """),
    "Plants" -> Deck("""
      💐 🌸 💮 🪷 🏵️ 🌹 🥀 🌺
      🌻 🌼 🌷 🪻 🌱 🪴 🌲 🌳
      🌴 🌵 🌾 🌿 ☘️ 🍀 🍁 🍂
      🍃 🍄 🪨 🪵
    """)
  )

  // Use any strings you want here — the tests don't check for these specific emoji
  val CARDS: Vector[String] = DECKS("Simple")

  /** Creates a new application state. */
  override def init(clients: Seq[UserId]): MemoryState =

    val init_board: Seq[(Card, CardView)] = CARDS
      .map(card =>
        (card, CardView.FaceDown) ::
          (card, CardView.FaceDown) :: Nil
      )
      .flatten
      .toSeq

    MemoryState.InGame(
      MemoryBoard(Random.shuffle(init_board), init_board.length),
      PhaseView.SelectingCards,
      clients.map(client => (client, Seq())).toMap,
      Seq(),
      clients,
      0
    )

  override def transition(
      state: MemoryState
  )(userId: UserId, event: MemoryEvent): Try[Seq[Action[MemoryState]]] =
    state match
      case MemoryState.OutofGame(winnerIds, scores) =>
        Try { throw new IllegalMoveException("Game is finished") }
      case MemoryState.InGame(
            board,
            phase,
            scores,
            selectedCards,
            clients,
            clientId
          ) =>
        event match
          case MemoryEvent.Toggle(cardId) =>
            val selectedCard = board.board(cardId)
            val selectedCardsSet = selectedCards.toSet

            if selectedCardsSet contains cardId then

              val newBoard = board.updateBoard(
                (selectedCard._1, cardId),
                CardView.FaceDown,
                board.nRemaining
              )

              val newSelected = (selectedCardsSet - cardId).toSeq
              Try {
                Seq(
                  Action.Render(
                    MemoryState.InGame(
                      newBoard,
                      PhaseView.SelectingCards,
                      scores,
                      newSelected,
                      clients,
                      clientId
                    )
                  )
                )
              }
            else

              val newBoard = board.updateBoard(
                (selectedCard._1, cardId),
                CardView.Selected,
                board.nRemaining
              )
              val newSelected = selectedCards :+ cardId
              if newSelected.length > 2 then
                Try {
                  throw new IllegalMoveException("Selected more than 2 cards")
                }
              else
                Try {
                  Seq(
                    Action.Render(
                      MemoryState.InGame(
                        newBoard,
                        PhaseView.SelectingCards,
                        scores,
                        newSelected,
                        clients,
                        clientId
                      )
                    )
                  )
                }
          case MemoryEvent.FlipSelected =>
            val firstCardId = selectedCards(0)
            val secondCardId = selectedCards(1)

            val firstCard = board.board(firstCardId)._1
            val secondCard = board.board(secondCardId)._1

            val faceUpBoard = board
              .updateBoard(
                (firstCard, firstCardId),
                CardView.FaceUp(firstCard),
                board.nRemaining
              )
              .updateBoard(
                (secondCard, secondCardId),
                CardView.FaceUp(secondCard),
                board.nRemaining
              )

            if firstCard == secondCard then
              val goodMatchBoard = board
                .updateBoard(
                  (firstCard, firstCardId),
                  CardView.AlreadyMatched(firstCard),
                  board.nRemaining
                )
                .updateBoard(
                  (secondCard, secondCardId),
                  CardView.AlreadyMatched(secondCard),
                  board.nRemaining - 2
                )

              val newScore = updateScore(scores, userId, firstCard)

              if isGameFinished(goodMatchBoard) then
                val winners = computeWinners(newScore)
                Try {
                  Seq(
                    Action.Render(
                      MemoryState.InGame(
                        faceUpBoard,
                        PhaseView.GoodMatch,
                        scores,
                        selectedCards,
                        clients,
                        clientId
                      )
                    ),
                    Action.Pause(1000),
                    Action.Render(
                      MemoryState.OutofGame(winners, newScore)
                    )
                  )
                }
              else

                Try {
                  Seq(
                    Action.Render(
                      MemoryState.InGame(
                        faceUpBoard,
                        PhaseView.GoodMatch,
                        scores,
                        selectedCards,
                        clients,
                        clientId
                      )
                    ),
                    Action.Pause(1000),
                    Action.Render(
                      MemoryState.InGame(
                        goodMatchBoard,
                        PhaseView.GoodMatch,
                        newScore,
                        Seq.empty[Int],
                        clients,
                        clientId
                      )
                    )
                  )
                }
            else

              val badMatchBoard = board
                .updateBoard(
                  (firstCard, firstCardId),
                  CardView.FaceDown,
                  board.nRemaining
                )
                .updateBoard(
                  (secondCard, secondCardId),
                  CardView.FaceDown,
                  board.nRemaining
                )

              Try {
                Seq(
                  Action.Render(
                    MemoryState.InGame(
                      faceUpBoard,
                      PhaseView.BadMatch,
                      scores,
                      selectedCards,
                      clients,
                      clientId
                    )
                  ),
                  Action.Pause(1000),
                  Action.Render(
                    MemoryState.InGame(
                      badMatchBoard,
                      PhaseView.BadMatch,
                      scores,
                      Seq.empty[Int],
                      clients,
                      computeNextClientInd(clientId, clients)
                    )
                  )
                )
              }

  /** Function for updating the score of the game
    *
    * @param scores
    * @param userId
    * @param card
    */
  def updateScore(
      scores: Map[UserId, Seq[Card]],
      userId: UserId,
      card: Card
  ): Map[UserId, Seq[Card]] =
    val currentScore: Seq[Card] = scores.getOrElse(userId, Seq())
    scores + (userId -> (currentScore ++ Seq(card, card)))

  override def project(state: MemoryState)(userId: UserId): MemoryView =
    state match
      case MemoryState.OutofGame(winnerIds, scores) =>
        MemoryView(StateView.Finished(winnerIds), scores)
      case MemoryState.InGame(
            board,
            phase,
            scores,
            selectedCards,
            clients,
            clientId
          ) =>
        if isGameFinished(board) then
          val winners = computeWinners(scores)
          MemoryView(
            StateView.Finished(winners),
            scores
          )
        else
          val boardProjection = projectBoard(board)
          val currentPlayer = clients(clientId)
          if currentPlayer == userId then
            MemoryView(
              StateView.Playing(phase, currentPlayer, boardProjection),
              scores
            )
          else
            MemoryView(
              StateView
                .Playing(PhaseView.GoodMatch, currentPlayer, boardProjection),
              scores
            )

  /** Project the internal version of the board onto a viewable version
    *
    * @param board
    */
  def projectBoard(board: MemoryBoard): Seq[CardView] =
    board.board.map(_._2)

  /** Game finished if and only if every card is Already Matched (board counts
    * them in nRemaining)
    *
    * @param board
    */
  def isGameFinished(board: MemoryBoard): Boolean =
    board.nRemaining == 0

  /** Extracts the user with the highest score in a set of winners
    *
    * @param scores
    */
  def computeWinners(scores: Map[UserId, Seq[Card]]): Set[UserId] =
    val listOfScores = scores.toList
    val maxScore = listOfScores.map(_._2.length).max
    listOfScores
      .filter((userId, cards) => cards.length == maxScore)
      .map(_._1)
      .toSet

  /** @param clientId
    *   : index of the client list
    * @param clients
    *   : sequence of clients
    */
  def computeNextClientInd(clientId: Int, clients: Seq[UserId]): Int =
    val maxIndex = clients.length - 1
    if clientId == maxIndex then 0 else clientId + 1

// Server registration magic
class register:
  WebServer.register(MemoryStateMachine)
