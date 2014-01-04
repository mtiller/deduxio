package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

trait ConstraintGenerator {
  def valid(prob: Problem): Set[Constraint];
}
