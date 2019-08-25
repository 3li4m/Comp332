/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.frogsandtoads

import doodle.core._
import doodle.syntax._
import doodle.image._
import doodle.image.Image._
import org.mq.frogsandtoads
import org.mq.frogsandtoads.Main.runAnimation

import scala.collection.immutable

/**
  * A puzzle state is given as a 1-dimensional array of cell values.
  */
class PuzzleState private (board: Vector[PuzzleState.Cell], loc: Int) {

  import PuzzleState._

  val size = board.size
  val emptyLoc = loc

  def isTerminalState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Toad) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Frog)
  }

  def isInitialState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Frog) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Toad)
  }

  // FIXME you might want to add more methods here.
  def getBoardState(): Vector[PuzzleState.Cell] ={
    board
  }

}

/**
  * Companion object for the [[PuzzleState]] class, provides a public constructor.
  */
object PuzzleState {

  /**
    * Case class for case objects to represent the possible contents of a
    * cell in the puzzle.
    */
  sealed abstract class Cell
  case object Frog extends Cell
  case object Toad extends Cell
  case object Empty extends Cell

  /**
    * Construct a [[PuzzleState]] object in the initial state for a
    * puzzle with specified numbers of frogs and toads.
    *
    * @param frogs number of frogs to place on the left of the [[PuzzleState]]
    * to be constructed
    * @param toads number of toads top place on the right of the [[PuzzleState]]
    * to be constructed
    */
  def apply(frogs: Int, toads: Int): PuzzleState = {
    if (frogs <= 0 || frogs > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    if (toads <= 0 || toads > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    new PuzzleState(
      Vector.fill(frogs)(Frog) ++ Vector(Empty) ++
        Vector.fill(toads)(Toad),
      frogs
    )
  }

  def customApply (vector: Vector[PuzzleState.Cell]): PuzzleState ={
    new PuzzleState(vector,vector.indexWhere(x => x == Empty))
  }


  /**
    * Find a sequence of legal moves of the frogs and toads puzzle from a specified starting
    * [[PuzzleState]] to the terminal [[PuzzleState]].
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[PuzzleState]] objects passed through in the transit from
    * state `start` to the terminal state (inclusive). Returns the empty sequence if no solution
    * is found.
    *
    *
    *
    *
    * If the state start is actually the terminal state of the game then declare success and return.
    * Otherwise, for each move that is possible from state start, do the following:
    *
    * make that move to give a new state which we call next,
    *
    * make a recursive call solve(next) to try to find a solution to the problem starting from position next,
    *
    * if that call was successful then declare success and return,
    *
    * If none of the possible moves tried resulted in success then declare failure and return.
    */
  def solve(start: PuzzleState): Seq[PuzzleState] = {
    // FIXME add your frogs and toads solver code here.
    var next: PuzzleState = start
    /*if (start.isTerminalState()) {
      println("Success")
      Seq(start)
    }
    if (!frogCanSlide(start) && !frogCanJump(start) && !toadCanSlide(start) && !toadCanJump(start)) {
      println("Fail")
      Seq(Nil)
    }
    if (frogCanJump(start) || frogCanJump(start))
    {
       next = frogMovement(start)
    }
    else if (toadCanSlide(start) || toadCanJump(start))
    {
      next = toadMovement(start)
    }
    solve(next)
    Seq(next)
    */
    Seq(start) ++ Seq(frogMovement(start)) ++ Seq(toadMovement(frogMovement(start)))
  }


  /**
    * Call [[solve]] to generate a sequence of legal moves from a specified
    * starting [[PuzzleState]] to the terminal [[PuzzleState]]. Render each state in that solution as
    * an image and return the resulting sequence of images.
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[Image]] objects depicting the sequence of puzzle states
    * passed through in the transit from the `start` state to the terminal state.
    */
  def animate(start: PuzzleState): Seq[Image] = {
    // FIXME add your code here to generate the animation frame sequence.
    val background = Image.rectangle(start.size * 32, start.size * 4).fillColor(Color.black).strokeColor(Color.black).strokeWidth(4)
    println(drawBoardState(solve(start).head.getBoardState().toList))
    println(start.size)
    elHelpo(solve(start),background)
  }

  // best helper function ever
  def elHelpo(start: Seq[PuzzleState], background: Image): Seq[Image]={
    start match {
      case Nil => Nil
      case h::t => Seq(drawBoardState(h.getBoardState().toList) on background) ++ elHelpo(t, background)
    }
  }

  /**
    * Create an animation of a solution to the frogs and toads puzzle, starting from the initial
    * [[PuzzleState]] and ending at the terminal [[PuzzleState]].
    * 
    * @param frogs the number of frogs in the puzzle (between 1 and 10 inclusive)
    * @param toads the number of toads in the puzzle (between 1 and 10 inclusive)
    */
  def animate(frogs: Int, toads: Int): Seq[Image] =
    animate(PuzzleState(frogs, toads))


  //place inside antoher function called cellsToDraw which is passed a list of puzzelstate cells
  // and returns an image
  // draw  + drawBoardState THIS IS WRONG
  // draw beside or above for image
  def drawBoardState(board: List[Cell]): Image={
    def drawCells(cell : Cell): Image =
      cell match {
        case Frog => square(30).fillColor(Color.red)
        case Empty => square(30).fillColor(Color.white)
        case Toad => square(30).fillColor(Color.green)
      }
    board match {
      case Nil => empty
      case h::t =>  drawCells(h) beside drawBoardState(t)
    }
  }

  def frogCanSlide(start: PuzzleState): Boolean={
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()
    if (emptyIndex > 0 && boardState(emptyIndex - 1) == Frog && boardState(emptyIndex + 1) != Frog){true} else false
  }
  def frogCanJump(start: PuzzleState): Boolean={
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()
    if ((emptyIndex + 1) < start.size && emptyIndex > 0 && boardState(emptyIndex - 2) == Frog && boardState(emptyIndex - 1) == Toad){true} else false
  }

  def frogMovement(start: PuzzleState): PuzzleState = {
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()

    if (frogCanSlide(start)) {
      new PuzzleState(
        boardState.take(emptyIndex - 1) ++
          Vector(boardState(emptyIndex)) ++
          Vector(boardState(emptyIndex - 1)) ++
          boardState.drop(emptyIndex + 1),emptyIndex - 1)
    }
    else if(frogCanJump(start)) {
      new PuzzleState(
        boardState.take(emptyIndex - 2) ++
          Vector(boardState(emptyIndex )) ++
          Vector(boardState(emptyIndex - 1)) ++
          Vector(boardState(emptyIndex + 1)) ++ boardState.drop(emptyIndex + 1),emptyIndex-2)
    }
   else {
      start
    }
  }

  def toadCanSlide(start: PuzzleState): Boolean={
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()
    if (emptyIndex > 0 && boardState(emptyIndex + 1) == Toad){true} else false
  }
  def toadCanJump(start: PuzzleState): Boolean={
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()
    if ((emptyIndex + 1) < start.size && emptyIndex > 0 && boardState(emptyIndex + 2) == Toad && boardState(emptyIndex + 1) == Frog){true} else false
  }

  def toadMovement(start: PuzzleState): PuzzleState = {
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()

    if (toadCanSlide(start)) {
      new PuzzleState(
        boardState.take(emptyIndex) ++
          Vector(boardState(emptyIndex + 1)) ++
          Vector(boardState(emptyIndex)) ++
          boardState.drop(emptyIndex + 2),emptyIndex + 1)
    }
    else if (toadCanJump(start)) {
      new PuzzleState(
        boardState.take(emptyIndex) ++
          Vector(boardState(emptyIndex + 2)) ++
          Vector(boardState(emptyIndex + 1)) ++
          Vector(boardState(emptyIndex)) ++
          boardState.drop(emptyIndex + 3),emptyIndex + 2)
    }
    else{
      start
    }
  }

}

