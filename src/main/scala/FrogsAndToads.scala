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
import doodle.image._
import doodle.image.Image._

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
  def getBoardState(): Vector[PuzzleState.Cell] ={board}

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
    */
  def solve(start: PuzzleState): Seq[PuzzleState] = {
    start match {
      case start if start.isTerminalState() => println("Success")
        Seq(start)

      case start if toadCanSlide(start) && frogCanJump(start) => Seq(start) ++ solve(frogJump(start))
      case start if frogCanSlide(start) && toadCanJump(start) => Seq(start) ++ solve(toadJump(start))

      case start if frogCanSlide(start) && toadCanSlide(start) => if (frogCanJump(frogSlide(start)) && toadCanJump(frogSlide(start)))
      {Seq(start) ++ solve(toadSlide(start))}
      else {Seq(start) ++ solve(frogSlide(start))}

      case start if toadCanSlide(start) => Seq(start) ++ solve(toadSlide(start))
      case start if frogCanSlide(start) => Seq(start) ++ solve(frogSlide(start))
      case start if toadCanJump(start) => Seq(start) ++ solve(toadJump(start))
      case start if frogCanJump(start) => Seq(start) ++ solve(frogJump(start))

      case _ => println("Failed")
        Seq()
    }

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
    val background = Image.rectangle(start.size * 32, 40).fillColor(Color.black)
    def animateHelper(start: Seq[PuzzleState], background: Image): Seq[Image]={
      start match {
        case Nil => Nil
        case h::t => Seq(drawBoardState(h.getBoardState().toList) on background) ++ animateHelper(t, background)
      }
    }
    animateHelper(solve(start),background)
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

  //Movement
  def frogSlide(start: PuzzleState): PuzzleState = {
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()

    if (frogCanSlide(start)) {
      new PuzzleState(
        boardState.slice(0,emptyIndex-1) ++
          Vector(boardState(emptyIndex)) ++
          Vector(boardState(emptyIndex - 1)) ++
          boardState.slice(emptyIndex + 1,start.size),emptyIndex - 1)
    }
    else {start}
  }
  def frogJump(start: PuzzleState): PuzzleState = {
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()

    if (frogCanJump(start)) {
      new PuzzleState(
        boardState.slice(0,emptyIndex - 2) ++
          Vector(boardState(emptyIndex)) ++
          Vector(boardState(emptyIndex - 1)) ++
          Vector(boardState(emptyIndex - 2)) ++ boardState.slice(emptyIndex + 1,start.size),emptyIndex-2)
    }
    else {start}
  }
  def toadSlide(start: PuzzleState): PuzzleState = {
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()

    if (toadCanSlide(start)) {
      new PuzzleState(
        boardState.slice(0,emptyIndex) ++
          Vector(boardState(emptyIndex + 1)) ++
          Vector(boardState(emptyIndex)) ++
          boardState.slice(emptyIndex + 2,start.size),emptyIndex + 1)
    }
    else{start}
  }
  def toadJump(start: PuzzleState): PuzzleState = {
    val emptyIndex = start.emptyLoc
    val boardState = start.getBoardState()

    if (toadCanJump(start)) {
      new PuzzleState(
        boardState.slice(0,emptyIndex) ++
          Vector(boardState(emptyIndex + 2)) ++
          Vector(boardState(emptyIndex + 1)) ++
          Vector(boardState(emptyIndex)) ++
          boardState.slice(emptyIndex + 3,start.size),emptyIndex + 2)
    }
    else{start}
  }

  //Movement checks
  def frogCanSlide(start: PuzzleState):  Boolean={
    if (start.emptyLoc > 0 && start.getBoardState()(start.emptyLoc - 1) == Frog)
    {true} else {false}
  }
  def frogCanJump(start: PuzzleState):  Boolean={
    if (start.emptyLoc >= 2 && start.getBoardState()(start.emptyLoc - 2) == Frog && start.getBoardState()(start.emptyLoc - 1) == Toad)
    {true} else {false}
  }
  def toadCanSlide(start: PuzzleState):  Boolean={
    if (start.emptyLoc < start.size - 1 && start.getBoardState()(start.emptyLoc + 1) == Toad)
    {true} else {false}
  }
  def toadCanJump(start: PuzzleState):  Boolean={
    if (start.emptyLoc < start.size - 2 && start.getBoardState()(start.emptyLoc + 2) == Toad && start.getBoardState()(start.emptyLoc + 1) == Frog)
    {true} else {false}
  }
}

