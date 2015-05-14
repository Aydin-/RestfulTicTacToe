package com.aydin

import java.util.concurrent.Executors
import javax.ws.rs._
import javax.ws.rs.core.{Response, MediaType}

import com.aydin.model.{GameState, Move}
import akka.actor.{ActorSystem, Props, Actor}

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.concurrent.TrieMap

@Path("/game")
@Produces(Array(MediaType.APPLICATION_JSON))
@Consumes(Array(MediaType.APPLICATION_JSON))
class NoughtsResource() {
  var gameSeqNr = 0
  var games = TrieMap[Int, Board]()
  var leaderBoard = TrieMap[String, Int]()
  val system = ActorSystem("apeSystem")
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  @POST
  def createGame(@QueryParam("player1Id") player1: String, @QueryParam("player2Id") player2: String): Response = {

    games foreach {
      case (key, board) =>
        if ((board.getPlayer1 == player1.toInt && board.getPlayer2 == player2.toInt && !board.isOver) ||
            (board.getPlayer2 == player1.toInt && board.getPlayer1 == player2.toInt && !board.isOver) ||
            (player1 == player2))
            throw new WebApplicationException(Response.status(Response.Status.BAD_REQUEST) //I'm a teapot
              .entity("Could not create game - players already playing each other")
              .build())
    }

    gameSeqNr += 1
    system.actorOf(Props(new GameKeeper)) ! GameBoard(gameSeqNr.toString, new Board((-10 to -2).toList, player1.toInt, player2.toInt, player1.toInt))
    Response.status(Response.Status.CREATED).entity(gameSeqNr.toString).build()

  }

  @GET
  @Path("/{gameId}")
  def getGame(@PathParam("gameId") gameId: String): GameState = {
    games get gameId.toInt match {
      case Some(game) =>
        if (game.player1Wins)
          GameState(Option(game.getPlayer1.toString), true)
        else if (game.player2Wins)
          GameState(Option(game.getPlayer2.toString), true)
        else
          GameState(None, false)
      case None =>
        throw new WebApplicationException(Response.status(Response.Status.NOT_FOUND)
        .entity("Game does not exist")
        .build())
    }
  }


  @GET
  @Path("/leaderBoard")
  def getLeaderBoard(): List[Map[String, String]] = {
    var topList = List[Map[String, String]]()

    leaderBoard.toList sortBy (-_._2) foreach {
      case (leader: String, score: Int) =>
        if (topList.size < 10)
          topList = Map("Player" -> leader, "Score" -> score.toString) :: topList
        else
          return topList.reverse
    }

    topList.reverse
  }

  @PUT
  @Path("/{gameId}")
  def makeMove(@PathParam("gameId") gameId: String, move: Move): String = {
    games get gameId.toInt match {
      case Some(game) =>
        if (game.getTurn.toString != move.playerId) {
          throw new WebApplicationException(Response.status(Response.Status.BAD_REQUEST)
            .entity("Player cannot play out of turn")
            .build())
        }

        if (!game.isOver) {
          val board: Board = game.play(move)

          system.actorOf(Props(new GameKeeper)) ! GameBoard(gameId, board)

          if (board.player1Wins)
            system.actorOf(Props(new ScoreKeeper)) ! PlayerScore(board.getPlayer1.toString)
          else if (board.player2Wins)
            system.actorOf(Props(new ScoreKeeper)) ! PlayerScore(board.getPlayer2.toString)
        }
        ""
      case None =>
        throw new WebApplicationException(Response.status(Response.Status.NOT_FOUND)
          .entity("Game does not exist")
          .build())
    }

  }

  case class PlayerScore(player: String)

  case class GameBoard(game: String, board: Board)

  class ScoreKeeper extends Actor {
    def receive = {
      case PlayerScore(player) =>
        Future {
          leaderBoard.update(player, leaderBoard get player match {
            case Some(score) => score + 1
            case None => 1
          })
        }
    }
  }

  class GameKeeper extends Actor {
    def receive = {
      case GameBoard(game, board) =>
        Future {
          games.update(game.toInt, board)
        }
    }
  }

}