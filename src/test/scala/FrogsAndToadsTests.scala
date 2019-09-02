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

  //TEST If toad can slide into a cell
  it should "Frog should be able to slide" in {
    val a = customApply(Vector(Frog,Frog,Empty,Toad,Frog))
    assertResult(Vector(Frog,Empty,Frog,Toad,Frog))(frogSlide(a).getBoardState())
  }

  //TEST If frog can jump into a cell
  it should "Frog should be able to jump" in {
    val a = customApply(Vector(Frog,Frog,Toad,Empty,Toad))
    assertResult(Vector(Frog,Empty,Toad,Frog,Toad))(frogJump(a).getBoardState())
  }

  //TEST If toad can slide into empty cell
  it should "Toad should be able to slide" in {
    val a = customApply(Vector(Frog,Empty,Toad))
    assertResult(Vector(Frog,Toad,Empty))(toadSlide(a).getBoardState())
  }

  //TEST If toad can jump into a cell
  it should "Toad should be able to jump" in {
    val a = customApply(Vector(Frog,Empty,Frog,Toad))
    assertResult(Vector(Frog,Toad,Frog,Empty))(toadJump(a).getBoardState())
  }

  //TEST If the frog is unable to slide
  it should "Frog should not slide over toad" in {
    val a = customApply(Vector(Frog,Frog,Toad,Empty,Frog))
    assertResult(a.getBoardState())(frogSlide(a).getBoardState())
  }

  //TEST If the frog can jump over another frog
  it should "Frog should not be able to jump" in {
    val a = customApply(Vector(Frog,Frog,Empty,Toad,Frog))
    assertResult(a.getBoardState())(frogJump(a).getBoardState())
  }

  //TEST If toad is unable to slide
  it should "Toad should not be able to slide" in {
    val a = customApply(Vector(Toad,Empty,Frog,Toad,Toad))
    assertResult(a.getBoardState())(toadSlide(a).getBoardState())
  }

  //TEST If the toad can jump over another toad
  it should "Toad should not be able to jump" in {
    val a = customApply(Vector(Frog,Frog,Empty,Toad,Toad))
    assertResult(a.getBoardState())(toadJump(a).getBoardState())
  }

}
