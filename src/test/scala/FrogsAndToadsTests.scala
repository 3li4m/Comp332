/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the Frogs and Toads puzzle solver.
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.frogsandtoads

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FrogsAndToadsTests extends FlatSpec with Matchers {

  import PuzzleState._

  // Tests of an empty B-tree

  "An puzzle state with 5 frogs and 8 toads:" should
    "have 5 + 8 + 1 = 14 cells" in {
    assert(PuzzleState(5, 8).size == 14)
  }

  it should "have its empty cell at position 5" in {
    assertResult(5) {
      PuzzleState(5, 8).emptyLoc
    }
  }

  it should "be constructed in the initial puzzle state" in {
    assert(PuzzleState(5, 8).isInitialState())
  }

  it should "not be constructed in the terminal puzzle state" in {
    assert(!PuzzleState(5, 8).isTerminalState())
  }

  it should "complete successfully in terminal state" in {
    val a = customApply(Vector(Toad,Toad,Empty,Frog,Frog))
    println(solve(a))
  }

  //TEST If toad can slide into a cell
  it should "Frog should be able to slide" in {
    val a = customApply(Vector(Frog,Frog,Empty,Toad,Frog))
    println(s"Calling method Slide frog")
    println(frogMovement(a).getBoardState())
    assertResult(Vector(Frog,Empty,Frog,Toad,Frog))(frogMovement(a).getBoardState())
  }

  //TEST If frog can jump into a cell
  it should "Frog should be able to jump" in {
    val a = customApply(Vector(Frog,Frog,Frog,Frog,Toad,Empty,Frog,Toad,Toad,Frog))
    println(s"Calling method Jump frog")
    assertResult(Vector(Frog,Frog,Frog,Empty,Toad,Frog,Frog,Toad,Toad,Frog))(frogMovement(a).getBoardState())
    println(frogMovement(a).getBoardState())
  }

  //TEST If the frog can slide over another cell
  it should "Frog should not slide over toad" in {
    val a = customApply(Vector(Frog,Frog,Toad,Empty,Frog))
    assertResult(false)(frogCanSlide(a))
  }
  //TEST If the frog can jump over another cell
  it should "Frog should not be able to jump" in {
    val a = customApply(Vector(Frog,Frog,Empty,Toad,Frog))
    assertResult(false)(frogCanJump(a))
  }

  //TEST If toad can slide into empty cell
  it should "Toad should be able to slide" in {
    val a = customApply(Vector(Frog,Empty,Toad))
    println(s"Calling method slide toad")
    println(toadMovement(customApply(Vector(Frog,Empty,Toad))).getBoardState())
    assertResult(Vector(Frog,Toad,Empty))(toadMovement(a).getBoardState())
  }

  //TEST If toad can jump into a cell
  it should "Toad should be able to jump" in {
    val a = customApply(Vector(Frog,Empty,Frog,Toad))
    println(s"Calling method Jump toad")
    println(toadMovement(customApply(Vector(Frog,Empty,Frog,Toad))).getBoardState())
    assertResult(Vector(Frog,Toad,Frog,Empty))(toadMovement(a).getBoardState())
  }

  //TEST If the toad can slide over another cell
  it should "Toad should not slide over toad" in {
    val a = customApply(Vector(Toad,Empty,Frog,Toad,Toad))
    assertResult(false)(toadCanSlide(a))
  }
  //TEST If the toad can jump over another cell
  it should "Toad should not be able to jump" in {
    val a = customApply(Vector(Frog,Frog,Empty,Toad,Toad))
    assertResult(false)(toadCanJump(a))
  }

}
