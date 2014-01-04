package com.xogeny.puzzles.deduxio.tests.alpha

import org.scalatest.Suites
import com.xogeny.puzzles.deduxio.tests.alpha.cons._
import com.xogeny.puzzles.deduxio.tests.alpha.solver._
import com.xogeny.puzzles.deduxio.tests.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */
class AllTests extends Suites(
  new AllCons,
  new AllRepr,
  new AllSolver
) {

}
