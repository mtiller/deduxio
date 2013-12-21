package com.xogeny.puzzles.simd.tests

import org.scalatest.FunSuite
import com.xogeny.puzzles.simd._
import scala.util.Random

/**
 * Created by mtiller on 12/20/13.
 */
class PathSpec extends FunSuite {
  test("Random path generator") {
    val board = Board.random(4, 4, 0, 4, List(Red, Green, Blue, Purple))
    val cur = List(Red -> List(4), Green -> List(10), Blue -> List(14), Purple -> List(8))
    val paths = Paths.random(board, cur, Set(), (0 to board.spaces.length-1).toSet)
    Random.setSeed(250);
    println(paths)
  }
}
