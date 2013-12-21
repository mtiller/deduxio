package com.xogeny.puzzles.simd

import scala.util.Random

/**
 * Created by mtiller on 12/21/13.
 */
case class Paths(map: Map[Color,List[Int]]);

case object Paths {
  type CurList = List[Pair[Color,List[Int]]];
  def random(board: Board, cur: CurList, dead: Set[Color], left: Set[Int]): Paths = left.size match {
    /* If all squares are on a path, we are done */
    case 0 => Paths(Map() ++ cur)
    /* If not, look at the next color to extend */
    case _ => cur match {
      /* If that color is "dead" (cannot be extended), then move on to next element in list */
      case (c, path) :: r if dead.contains(c) => random(board, (cur tail) ::: ((cur head) :: Nil), dead, left)
      case (c, path) :: r => {
        val adjs = board.adjacent(path head) filter { left.contains(_) }
        if (adjs==Nil) {
          val tadjs = board.adjacent(path last) filter { left.contains(_) }
          if (tadjs==Nil) {
            val newdead = dead + c;
            /* No more colors we can process, we're done */
            if (newdead.size==cur.length) Paths(Map() ++ cur);
            else random(board, (cur tail) ::: ((cur head) :: Nil), newdead, left);
          }
          else {
            val choice = Random.shuffle(tadjs.toList).head
            random(board, (cur tail) ::: ((c, path ::: (choice :: Nil)) :: Nil), dead, left-choice)
          }
        }
        else {
          val choice = Random.shuffle(adjs.toList).head
          random(board, (cur tail) ::: ((c, choice :: path) :: Nil), dead, left-choice)
        }
      }
    }
  }
}
