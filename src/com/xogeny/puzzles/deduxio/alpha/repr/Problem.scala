package com.xogeny.puzzles.deduxio.alpha.repr

/**
 * Created by mtiller on 1/3/14.
 */

/**
 * This class represents a given puzzle
 * @param board The board of the problem
 * @param vars The variables of the problem
 */
case class Problem(board: Board, vars: Set[String]);
