package com.aydin

import javax.ws.rs.core.Response.Status

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.aydin.model.{Move, GameState}
import io.dropwizard.testing.junit.DropwizardAppRule
import org.scalatest.junit.JUnitSuite

import org.junit.Test
import org.junit.ClassRule
import com.mashape.unirest.http.Unirest
import org.scalatest.Matchers


object TicTacToeTest {
  @ClassRule def rule = new DropwizardAppRule[TicTacConfiguration](classOf[TicTacApplication], "test.yml")
}

class TicTacToeTest extends JUnitSuite with Matchers {

  val baseUrl = "http://localhost:8080/game"

  val objectMapper = new ObjectMapper()
  objectMapper.registerModule(DefaultScalaModule)

  def initGame(player1Id: String, player2Id: String) = {
    val response = Unirest.post(baseUrl)
      .queryString("player1Id", player1Id)
      .queryString("player2Id", player2Id)
      .asString()

    if (response.getStatus != Status.CREATED.getStatusCode && response.getStatus != Status.BAD_REQUEST.getStatusCode) {
      throw new RuntimeException(s"${response.getStatus} when creating game: ${response.getBody}")
    }

    if(response.getStatus == Status.CREATED.getStatusCode)
      objectMapper.readValue(response.getBody, classOf[String])
    else
      response.getBody
  }

  def runMoves(gameId: String, moves: Seq[Move]) = {
    moves.foreach(move => {
      val response = Unirest.put(s"$baseUrl/$gameId")
        .header("Content-Type", "application/json")
        .body(objectMapper.writeValueAsString(move))
        .asString()

      if (response.getStatus != Status.OK.getStatusCode && response.getStatus != Status.BAD_REQUEST.getStatusCode) {
        throw new RuntimeException(s"${response.getStatus} when making move: ${response.getBody}")
      }

    })
  }

  def getState(gameId: String) = {
    val response = Unirest.get(s"$baseUrl/$gameId").asString()

    if (response.getStatus != Status.OK.getStatusCode) {
      throw new RuntimeException(s"${response.getStatus} when getting state: ${response.getBody}")
    }

    objectMapper.readValue(response.getBody, classOf[GameState])
  }

  @Test
  def testPlayer1Win {
    val gameId = initGame("1", "2")
    runMoves(gameId, Seq(
      Move("1", 0, 0),
      Move("2", 1, 0),
      Move("1", 0, 1),
      Move("2", 1, 1),
      Move("1", 0, 2)))

    getState(gameId) should be(GameState(Some("1"), true))

  }

  @Test
  def testPlayer2Win {
    val gameId = initGame("3", "4")
    runMoves(gameId, Seq(
      Move("3", 2, 2),
      Move("4", 0, 0),
      Move("3", 1, 0),
      Move("4", 0, 1),
      Move("3", 1, 1),
      Move("4", 0, 2)))

    getState(gameId) should be(GameState(Some("4"), true))

  }

  @Test
  def testMoveOutOfTurn {
    val gameId = initGame("7", "8")
    runMoves(gameId, Seq(
      Move("7", 2, 2),
      Move("8", 0, 0)
    ))

    val response = Unirest.put(s"$baseUrl/$gameId")
      .header("Content-Type", "application/json")
      .body(objectMapper.writeValueAsString(Move("8",1,1)))
      .asString()

    response.getStatus should be (Status.BAD_REQUEST.getStatusCode)
    response.getBody should be ("Player cannot play out of turn")
  }

  @Test
  def testGameNotAllowed {
    Unirest.post(baseUrl)
      .queryString("player1Id", "5")
      .queryString("player2Id", "6")
      .asString().getStatus should be (Status.CREATED.getStatusCode)

    initGame("5", "6") should be("Could not create game - players already playing each other")
    initGame("6", "6") should be("Could not create game - players already playing each other")
    initGame("6", "7") shouldNot be("Could not create game - players already playing each other")

  }

  @Test
  def testGameNotFound {
    Unirest.get(s"$baseUrl/9999").asString().getStatus should be (Status.NOT_FOUND.getStatusCode)
  }

  @Test
  def testLeaderBoard {

    for (player <- 10 to 80) {
      winAGame(player.toString, (player + 1).toString)

      if (player % 2 == 0) {
        winAGame(player.toString, (player + 1).toString)
      }

      if (player % 10 == 0) {
        winAGame(player.toString, (player + 1).toString)
      }

      if (player % 7 == 0) {
        winAGame(player.toString, (player + 1).toString)
      }

      if (player % 5 == 0) {
        winAGame(player.toString, (player + 1).toString)
      }

    }

    val response = Unirest.get(s"$baseUrl/leaderBoard")
      .header("Content-Type", "application/json")
      .asJson()

    if (response.getStatus != Status.OK.getStatusCode) {
      throw new RuntimeException(s"${response.getStatus} when getting leaderBoard: ${response.getBody}")
    }

    //println(response.getBody.toString)

    val firstPlacePlayer = "70"
    val secondPlaceScore = "4"

    response.getBody.getArray.getJSONObject(0).get("Player") should be(firstPlacePlayer)
    response.getBody.getArray.getJSONObject(1).get("Score") should be(secondPlaceScore)

  }

  def winAGame(player1: String, player2: String) {
    runMoves(initGame(player1, player2), Seq(
      Move(player1, 0, 0),
      Move(player2, 1, 0),
      Move(player1, 0, 1),
      Move(player2, 1, 1),
      Move(player1, 0, 2)))
  }

  def winAnotherGame(player1: String, player2: String) {
    runMoves(initGame(player1, player2), Seq(
      Move(player1, 0, 0),
      Move(player2, 1, 0),
      Move(player1, 1, 1),
      Move(player2, 2, 1),
      Move(player1, 2, 2)))
  }

  @Test
  def testConcurrent {
    val player1 = "1000"
    val player2 = "1001"
    val game = initGame(player1, player2)
    val thread1 = new Thread {
      override def run {
        for (p <- 100 to 105) {
          runMoves(game, Seq(
            Move(player1, 1, 1),
            Move(player2, 2, 1),
            Move(player1, 2, 2)))

          for (i <- 1 to 5) {
            winAGame(p.toString, (p + 1).toString)
            winAnotherGame(p.toString, (p + 1).toString)
          }
        }
      }
    }

    val thread2 = new Thread {
      override def run {
        for (player <- 100 to 105) {
          {
              runMoves(game, Seq(
                Move(player1, 0, 0),
                Move(player2, 1, 0)))
              winAGame(player.toString, (player + 2).toString)
          }
        }
      }
    }

    thread1.run
    thread2.run

    val response = Unirest.get(s"$baseUrl/leaderBoard")
      .header("Content-Type", "application/json")
      .asJson()

    if (response.getStatus != Status.OK.getStatusCode) {
      throw new RuntimeException(s"${response.getStatus} when getting leaderBoard: ${response.getBody}")
    }

    val expectedScore = "11"

    for (i <- 0 to 5) {
      response.getBody.getArray.getJSONObject(i).get("Score") should be(expectedScore)
    }

    getState(game) should be(GameState(Some("1000"), true))
  }

}
