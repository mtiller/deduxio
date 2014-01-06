package com.xogeny.puzzles.deduxio.alpha.render

import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/5/14.
 */
object JSONRenderer {
  def render(board: Board, cons: List[Constraint], sol: Map[String,Int]) = {
    val bobj = renderBoard(board);
    val cobj = renderConstraints(cons);
    val sobj = renderSolution(sol);
    s"""
{"board": $bobj,
 "cons": $cobj,
 "sol": $sobj}"""
  }
  private def renderSolution(sol: Map[String,Int]) = {
    val sstr = sol map { p => s""" "${p._1}": ${p._2} """}
    s"""
    {${sstr mkString ",\n     "}}"""
  }
  private def renderBoard(board: Board) = {
    val spaces = board.spaces map { renderSpace(_) }
    s"""
    [${spaces mkString ",\n     "}]"""
  }
  private def renderSpace(s: Space) = {
    s"""{"number": ${s.number}, "color": ${s.color}, "row": ${s.y}, "column": ${s.x}}"""
  }
  private def renderConstraints(cons: List[Constraint]) = {
    val cstrs = cons map { renderConstraint(_, invert=false) }
    s"""
    [${cstrs mkString ",\n     "}]"""
  }
  private def renderConstraint(c: Constraint, invert: Boolean): String = {
    val addProp = c match {
      case IsColor(v, color) => s""", "color": "${color}" """
      case IsColumn(v, column) => s""", "column": ${column}"""
      case IsNumber(v, n) => s""", "number": ${n}"""
      case IsOnPath(v, color) => s""", "path": "${color}" """
      case IsRow(v, r) => s""", "row": ${r}"""
      case _ => ""
    }
    c match {
      case PrimaryNot(x) => renderConstraint(x, invert=true);
      case SecondaryNot(x) => renderConstraint(x, invert=true);
      case p: PrimaryConstraint => s"""{"type": "${p.getClass.getSimpleName}", "v": "${p.v}", "invert": ${invert}${addProp}}"""
      case s: SecondaryConstraint => s"""{"type": "${s.getClass.getSimpleName}", "v1": "${s.v1}", "v2": "${s.v2}", "invert": ${invert}${addProp}}"""
    }
  }
}
