package com.xogeny.puzzles.simd

import scala.util.Random

/**
 * Created by mtiller on 12/24/13.
 */

trait Tweaker {
  def adjustPriority(c: SimdConstraint): Int;
}

case object NullTweaker extends Tweaker {
  def adjustPriority(c: SimdConstraint): Int = 0;
}

case object CircularTweaker extends Tweaker {
  def adjustPriority(c: SimdConstraint): Int = c match {
    case p: PrimaryConstraint => 30;
    case _ => 0;
  };
}

case object TreeTweaker {
  private def process(vars: List[String]): List[(String,String)] = vars match {
    case x :: a :: b :: r => {  /* x depends on a and b */
      val head = List(x -> a, x -> b)
      r match {
        case Nil => (x -> a) :: (x -> b) :: Nil
        case _ => {
          val tlen = r.length
          val n = Random.nextInt(tlen)
          val atail = r take n
          val btail = r drop n
          head ::: process(a :: atail) ::: process(b :: btail)
        }
      }
    }
    case x :: a :: Nil => {  /* x depends only on a */
      (x, a) :: Nil
    }
    case x :: Nil => Nil  /* x is a leaf */
    case Nil => Nil /* done */
  }
  def randomPlan(vars: List[String]) = {
    process(Random.shuffle(vars));
  }
}
case class TreeTweaker(plan: List[Pair[String,String]]) extends Tweaker {
  def isLeaf(s: String) = plan forall { p => p._1 != s }
  def parent(s: String) = plan filter { p => p._2==s } map { _._1 }
  def dependsOn(s1: String, s2: String): Boolean = plan exists { p => p._1==s1 && p._2==s2 }
  def adjustPriority(c: SimdConstraint): Int = {
    val la = c match {
      case p: PrimaryConstraint if isLeaf(p.ball) => -30
      case p: PrimaryConstraint => 20
      case _ => 0
    }
    val da = c match {
      case s: SecondaryConstraint if dependsOn(s.b1, s.b2) => -30
      case s: SecondaryConstraint => 40
      case _ => 10
    }
    val na = if (c.isNegative) 30 else 0;
    la + da + na
  }
}
