package com.xogeny.puzzles.deduxio.tests.alpha

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

object Samples {
  val P1 = Paths(Map(Blue -> List(0, 1, 2), Green -> List(3, 4, 5), Red -> List(6, 7, 8)));
  val B1 = Board(List(Space(Red,   1, 1, 1), Space(Red,   2, 2, 1), Space(Red,   3, 3, 1),
                      Space(Blue,  1, 1, 2), Space(Blue,  2, 2, 2), Space(Blue,  3, 3, 2),
                      Space(Green, 1, 1, 3), Space(Green, 2, 2, 3), Space(Green, 3, 3, 3)), P1);
}
