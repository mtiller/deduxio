package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/4/14.
 */

sealed abstract class SecondaryConstraint(val v1: String, val v2: String) extends Constraint {
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

sealed abstract class PositiveSecondaryConstraint(V1: String, V2: String) extends SecondaryConstraint(V1, V2);

case object AdjacentTo extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.adjacent(s2)) List(AdjacentTo(v1, v2)) else Nil
}
case class AdjacentTo(V1: String, V2: String) extends PositiveSecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.adjacent(s2)
}

case object GreaterThan extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.number>s2.number) List(GreaterThan(v1, v2)) else Nil
}
case class GreaterThan(V1: String, V2: String) extends PositiveSecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.number>s2.number;
}

case object LessThan extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.number<s2.number) List(LessThan(v1, v2)) else Nil
}
case class LessThan(V1: String, V2: String) extends PositiveSecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.number<s2.number;
}

case object SameColor extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.color==s2.color) List(SameColor(v1, v2)) else Nil
}
case class SameColor(V1: String, V2: String) extends PositiveSecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.color==s2.color;
}

case object SameColumn extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.x==s2.x) List(SameColumn(v1, v2)) else Nil
}
case class SameColumn(V1: String, V2: String) extends PositiveSecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.x==s2.x;
}

case object SameNumber extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.number==s2.number) List(SameNumber(v1, v2)) else Nil
}
case class SameNumber(V1: String, V2: String) extends PositiveSecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.number==s2.number;
}

case object SamePath extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = (board.paths(s1) & board.paths(s2)).size match {
    case 0 => Nil
    case _ => List(SamePath(v1, v2));
  }
}
case class SamePath(V1: String, V2: String) extends PositiveSecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = (board.paths(s1) & board.paths(s2)).size>0
}

case object SameRow extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.y==s2.y) List(SameRow(v1, v2)) else Nil
}
case class SameRow(V1: String, V2: String) extends PositiveSecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = s1.y==s2.y;
}

case class SecondaryNot(c: PositiveSecondaryConstraint) extends SecondaryConstraint(c.v1, c.v2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = !c.satisfies(board, s1, s2);
}

/* Negative Secondary Generators */
case object NotAdjacentTo extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (!s1.adjacent(s2)) List(SecondaryNot(AdjacentTo(v1, v2))) else Nil
}

case object NotSameColor extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.color!=s2.color) List(SecondaryNot(SameColor(v1, v2))) else Nil
}

case object NotSameColumn extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.x!=s2.x) List(SecondaryNot(SameColumn(v1, v2))) else Nil
}

case object NotSameNumber extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.number!=s2.number) List(SecondaryNot(SameNumber(v1, v2))) else Nil
}

case object NotSameRow extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = if (s1.y!=s2.y) List(SecondaryNot(SameRow(v1, v2))) else Nil
}
