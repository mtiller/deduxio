package com.xogeny.puzzles.simd

import JaCoP.constraints._
import java.util
import scala.Some

/**
 * Created by mtiller on 12/14/13.
 */

trait ConstraintGenerator {
  def allValid(board: Board, sol: Map[String, Int]): List[SimdConstraint];
}

object SimdConstraint extends ConstraintGenerator {
  def allValid(board: Board, sol: Map[String,Int]): List[SimdConstraint] = {
    IsNumber.allValid(board, sol) ::: IsColor.allValid(board, sol) ::: SameColor.allValid(board, sol)
  }
}

abstract class SimdConstraint {
  def constraints(prob: Problem): List[Constraint];
}

abstract class PrimaryConstraint(ball: String) extends SimdConstraint {
  def satisfies(s: Space): Boolean;
  def constraints(prob: Problem): List[Constraint] = {
    val candidates = ((0 to (prob.board.spaces.length-1)).toList filter { s => satisfies(prob.board.spaces(s)) }).toArray
    val eqs = candidates map { new XeqC(prob.ball(ball), _) }
    if (eqs.length==1) List(eqs(0))
    else {
      var ret = new util.ArrayList[PrimitiveConstraint];
      eqs foreach { ret.add(_) }
      List(new Or(ret));
    }
  }
}


abstract class SecondaryConstraint(b1: String, b2: String) extends SimdConstraint {
  def constraints(prob: Problem): List[Constraint] = {
    val spaces = 0 to (prob.board.spaces.length-1);
    val candidates = for(i <- spaces;
                         j <- spaces;
                         if i!=j;
                         si <- Some(prob.board.spaces(i));
                         sj <- Some(prob.board.spaces(j));
                         if satisfies(si,sj)) yield new And(new XeqC(prob.ball(b1), i), new XeqC(prob.ball(b2), j))
    if (candidates.length==1) List(candidates(0))
    else {
      var ret = new util.ArrayList[PrimitiveConstraint];
      candidates foreach { ret.add(_) }
      List(new Or(ret));
    }
  }
  def satisfies(s1: Space, s2: Space): Boolean;
}

