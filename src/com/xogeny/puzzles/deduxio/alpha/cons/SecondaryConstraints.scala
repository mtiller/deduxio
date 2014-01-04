package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/4/14.
 */

abstract class SecondaryConstraint(val v1: String, val v2: String) extends Constraint {
  def involves(i1: String, i2: String) = (i1==v1 && i2==v2) || (i1==v2 && i2==v1)
  def satisfies(board: Board, s1: Space, s2: Space): Boolean;
  def evaluate(board: Board): Set[(Int,Int)] = (for (s1 <- board.elements;
                                                     s2 <- board.elements;
                                                     if s1!=s2;
                                                     if satisfies(board, s1._2, s2._2)) yield (s1._1, s2._1)).toSet;
}

trait SecondaryConstraintGenerator extends ConstraintGenerator {
  def valid(prob: Problem, sol: Map[String,Int]): Set[Constraint] = {
    for (v1 <- prob.vars;
         v2 <- prob.vars;
         if v1!=v2;
         s1 <- (sol.get(v1) map { prob.board.spaces(_) }).toList;
         s2 <- (sol.get(v2) map { prob.board.spaces(_) }).toList;
         if s1!=s2;
         c <- generate(prob.board, v1, s1, v2, s2)) yield c;
  }
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space): List[SecondaryConstraint];
}

case class AdjacentTo(V1: String, V2: String) extends SecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = {
    (s1.x+1==s2.x && s1.y==s2.y) ||
    (s1.x-1==s2.x && s1.y==s2.y) ||
    (s1.x==s2.x && s1.y+1==s2.y) ||
    (s1.x==s2.x && s1.y-1==s2.y)
  }
}

case class GreaterThan(V1: String, V2: String) extends SecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.number>s2.number;
}

case class LessThan(V1: String, V2: String) extends SecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.number<s2.number;
}

case class SameColor(V1: String, V2: String) extends SecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.color==s2.color;
}

case class SameColumn(V1: String, V2: String) extends SecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.x==s2.x;
}

case class SameNumber(V1: String, V2: String) extends SecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.number==s2.number;
}

case object SamePath extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = (board.paths(s1) & board.paths(s2)).size match {
    case 0 => Nil
    case _ => List(SamePath(v1, v2));
  }
}

case class SamePath(V1: String, V2: String) extends SecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = (board.paths(s1) & board.paths(s2)).size>0
}

case class SameRow(V1: String, V2: String) extends SecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.y==s2.y;
}
