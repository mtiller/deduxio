package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.cons._

/**
 * Created by mtiller on 1/4/14.
 */

/**
 * This trait is used to implement scoring algorithms
 */
trait Scorer {
  /**
   * For a given constraint, return the score
   * @param c Constraint
   * @return Score (higher - more likely, lower - less likely)
   */
  def score(c: Constraint): Int;
}

/**
 * This scorer prefers SecondaryConstraints over PrimaryConstraints
 */
case object PvS extends Scorer {
  def score(c: Constraint) = c match {
    case p: PrimaryConstraint => -5;
    case _ => 5;
  }
}

/**
 * A tree scorer assumes a list of variable pairs and will encourage secondary constraints with those
 * pairs and discourage pairs that are not present.  It also avoid inverted constraints and primary constraints
 * on variables "leaf" variables (those with only one degree in the resulting graph).
 * @param tree Information about the tree to use as the solution "plan"
 */
case class TreeScorer(tree: List[(String,String)]) extends Scorer {
  /**
   * Computes the degree for a given variable
   * @param s Variable
   * @return Degree
   */
  def degree(s: String) = {
    val others = tree flatMap { p => if (p._1==s) Some(p._2) else if (p._2==s) Some(p._2) else None }
    others.toSet.size
  }

  /**
   * Scores a given constraint on the basis of the solution plan
   * @param c Constraint
   * @return Score (higher - more likely, lower - less likely)
   */
  def score(c: Constraint) = c match {
    case pn: PrimaryNot => -10;   // commenting this out makes problems a bit more difficult (I think)
    case p: PrimaryConstraint if degree(p.v)==1 => 5;
    case p: PrimaryConstraint => -5;
    case s: SecondaryConstraint if tree.exists { p => p._1==s.v1 && p._2==s.v2 || p._1==s.v2 && p._2==s.v1 } => 20;
    case _ => 0;
  }
}
