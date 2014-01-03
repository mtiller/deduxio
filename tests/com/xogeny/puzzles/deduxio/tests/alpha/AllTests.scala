package com.xogeny.puzzles.deduxio.tests.alpha

import org.scalatest.Suites

/**
 * Created by mtiller on 1/3/14.
 */
class AllTests extends Suites(
  new BoardTests,
  new IsNumberTests,
  new TestSetSolver
) {

}
