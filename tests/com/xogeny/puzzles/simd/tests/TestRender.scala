package com.xogeny.puzzles.simd.tests

import org.scalatest.FunSuite
import com.xogeny.puzzles.simd._
import scala.reflect.io.File

/**
 * Created by mtiller on 12/23/13.
 */
class TestRender extends FunSuite {
  test("Rendering of a sample board") {
    val board = Board.random(5, 5, 0, 5, List(Red, Blue, Green, Purple, Yellow))
    val html = SVGRender.render(board, Nil, Map());
    File("sample_puzzle.html").writeAll(html)
  }
}
