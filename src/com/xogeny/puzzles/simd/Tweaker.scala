package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/24/13.
 */

trait Tweaker {
  def adjustPriority(c: SimdConstraint): Int;
}

case object NullTweaker extends Tweaker {
  def adjustPriority(c: SimdConstraint): Int = 0;
}

//case class DTree(cur: List[Pair[String,String]]) {
//  def solved: List[String] = (cur flatMap { p => List(p._1, p._2)}).distinct;
//  def conforms(cons: List[SimdConstraint]): Boolean = {
//    cur forall { p =>
//      cons exists {
//        case s: SecondaryConstraint => {
//          //println(s"looking for ${s.b1} and ${s.b2} in ${p._1} and ${p._2}")
//          val ret = (s.b1==p._1 && s.b2==p._2) || (s.b1==p._2 && s.b2==p._1)           //println(ret);
//          ret
//        }
//        case _ => false
//      }
//    }
//  }
//}

case class TreeTweaker(plan: List[Pair[String,String]]) extends Tweaker {
  def isLeaf(s: String) = plan forall { p => p._1 != s }
  def dependsOn(s1: String, s2: String): Boolean = plan exists { p => p._1==s1 && p._2==s2 }
  def adjustPriority(c: SimdConstraint): Int = c match {
    case p: PrimaryConstraint => if (isLeaf(p.ball)) -30 else +20
    case s: SecondaryConstraint => if (dependsOn(s.b1, s.b2)) -30 else +20
  }
}
