package com.xogeny.puzzles.deduxio.tests.alpha

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

case object P1 extends Paths(Map(Blue -> List(1, 2, 3), Green -> List(4, 5, 6), Blue -> List(7, 8, 9)));
case object B1 extends Board(List(Space(Red, 1, 1, 1), Space(Red, 2, 2, 1), Space(Red, 3, 3, 1),
                                  Space(Blue, 1, 1, 2), Space(Blue, 2, 2, 2), Space(Blue, 3, 3, 2),
                                  Space(Green, 1, 1, 3), Space(Green, 2, 2, 3), Space(Green, 3, 3, 3)), P1);
