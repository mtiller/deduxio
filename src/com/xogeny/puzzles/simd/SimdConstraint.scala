package com.xogeny.puzzles.simd

import JaCoP.constraints._
import java.util
import scala.Some

/**
 * Created by mtiller on 12/14/13.
 */

trait ConstraintGenerator[T <: SimdConstraint] {
  def allValid(board: Board, sol: Map[String, Int]): List[T];
  def allValidPairsWhere(board: Board, sol: Map[String, Int])(f: (String, Space, String, Space) => Option[T]): List[T] = {
    val names = sol.keys.toList
    for(i <- names;
        j <- names;
        si <- sol.get(i) map { board.spaces(_) };
        sj <- sol.get(j) map { board.spaces(_) };
        if i!=j;
        con <- f(i, si, j, sj)) yield con
  }
}

object SimdConstraint extends ConstraintGenerator[SimdConstraint] {
  def allValid(board: Board, sol: Map[String,Int]): List[SimdConstraint] = {
    allValidPrimary(board, sol) ::: allValidSecondary(board, sol)
  }
  def allValidPrimary(board: Board, sol: Map[String,Int]): List[PrimaryConstraint] = {
    val generators = List(IsNumber, /* IsNotNumber, */ IsColor, /*IsNotColor,*/
                          IsOnPath, IsRow /*, IsNotRow */ /*, IsNotColumn*/)
    generators flatMap { _.allValid(board, sol) }
  }
  def allValidSecondary(board: Board, sol: Map[String,Int]): List[SecondaryConstraint] = {
    val generators = List(SameColor, NotSameColor, SameNumber, NotSameNumber,
                          LessThan, GreaterThan, IsOnPathWith, IsNotOnPathWith, SameRow, SameCol,
                          DifferentRow, DifferentColumn, AdjacentTo, NotAdjacentTo)
    generators flatMap { _.allValid(board, sol) }
  }
}

abstract class SimdConstraint(val priority: Int) {
  def constraints(prob: Problem): List[Constraint];
}


