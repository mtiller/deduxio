package com.xogeny.puzzles.deduxio.tests.alpha.cons

import org.scalatest.Suites

/**
 * Created by mtiller on 1/4/14.
 */
class AllCons extends Suites(
  new TestIsColor,
  new TestIsNumber,
  new TestIsRow,
  new TestIsColumn,
  new TestPrimaryNot
) {

}
