package com.aydin

import com.aydin.model.{GameState, Move}

/**
 * Created by aydin.gungordu on 09/04/15.
 */

class Board(myBoard: List[Int], player1: Int, player2: Int, turn: Int) {

  val WinningCombos = List((-10, -9, -8), (-7, -6, -5), (-4, -3, -2), (-10, -7, -4), (-9, -6, -3), (-8, -5, -2), (-10, -6, -2), (-8, -6, -4))
  val CoordMap = Map((0, 0) -> -10, (0, 1) -> -9, (0, 2) -> -8, (1, 0) -> -7, (1, 1) -> -6, (1, 2) -> -5, (2, 0) -> -4, (2, 1) -> -3, (2, 2) -> -2)

  var gameState = GameState(None, false)

  def player2Plays(move: Int) = {
    if (myBoard.indexOf(move) == -1 || isOver)
     this
    else
     new Board(myBoard updated(myBoard.indexOf(move), player2), player1, player2, player1)
  }

  def player1Plays(move: Int) = {
    if (myBoard.indexOf(move) == -1 || isOver)
      this
    else
      new Board(myBoard updated(myBoard.indexOf(move), player1), player1, player2, player2)
  }

  def isDraw = myBoard.forall(c => c == player1 || c == player2)

  def isOver = isDraw || player1Wins || player2Wins

  def isWinner(winner: Int) =
    WinningCombos.exists { case (i, j, k) => myBoard(i + 10) == winner && myBoard(j + 10) == winner && myBoard(k + 10) == winner }

  def player1Wins = isWinner(player1)

  def player2Wins = isWinner(player2)

  def getPlayer1 = player1
  def getPlayer2 = player2
  def getTurn = turn

  def play(move: Move) = {
    CoordMap get ((move.x, move.y)) match {
      case Some(m) =>
        if (move.playerId.toInt == player1 && turn == player1) player1Plays(m)
        else if (move.playerId.toInt == player2 && turn == player2) player2Plays(m)
        else this
      case None => this
    }
  }
}



