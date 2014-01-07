package com.xogeny.puzzles.deduxio.alpha.render

import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr.Board

/**
 * Created by mtiller on 1/5/14.
 */

/**
 * This object can render a given problem as SVG.
 */
object SVGRenderer {
  def file(svg: String) =
    s"""
<!DOCTYPE html>
<html>
  <head>
    <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/css/bootstrap-combined.min.css" rel="stylesheet">
    <script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/js/bootstrap.min.js"></script>
  </head>
  <body>
    <svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="960" height="1440">
      $svg
    </svg>
  </body>
</html>
"""
  def pathString(points: List[(Int,Int)]) = {
    points map { p => s"${p._1},${p._2}"} mkString " "
  }
  def path(points: List[(Int,Int)], color: String) = {
    val path = pathString(points)
    s"""
    <polyline points="$path" style="fill:none;stroke:black;stroke-width:12" />
    <polyline points="$path" style="fill:none;stroke:$color;stroke-width:10" />
    """
  }
  def row(x: Int, y: Int, w: Int) = {
    val p1 = (x-40, y+30) :: (x+w+40, y+30) :: Nil;
    val p2 = (x-40, y-30) :: (x+w+40, y-30) :: Nil;
    s"""
    <polyline points="${pathString(p1)}" style="fill:none;stroke:black;stroke-width:2" />
    <polyline points="${pathString(p2)}" style="fill:none;stroke:black;stroke-width:2" />
    """
  }
  def column(x: Int, y: Int, w: Int) = {
    val p1 = (x-40, y+30) :: (x-40, y-30) :: Nil;
    val p2 = (x+w+40, y+30) :: (x+w+40, y-30) :: Nil;
    s"""
    <polyline points="${pathString(p1)}" style="fill:none;stroke:black;stroke-width:2" />
    <polyline points="${pathString(p2)}" style="fill:none;stroke:black;stroke-width:2" />
    """
  }
  def ball(num: String, x: Int, y: Int, color: String) = {
    s"""
      <circle cx="$x" cy="$y" r="25" stroke="#808080" stroke-width="1" stroke-antialiasing="true" fill="$color" />
      <text fill="black" x="$x" y="${y+10}" font-size="32px"  style="text-anchor: middle;">$num</text>
    """
  }
  def ctext(s: String, x: Int, y: Int) = {
    s""" <text fill="black" x="$x" y="${y+10}" font-size="32px"  style="text-anchor: middle;">$s</text> """
  }
  def ltext(s: String, x: Int, y: Int) = {
    s""" <text fill="black" x="$x" y="${y+10}" font-size="32px"  style="text-anchor: left;">$s</text> """
  }
  def renderTwoBallsWithText(s1: String, s2: String, t: String, x: Int, y: Int, c1: String="#ffffff", c2: String="#ffffff") = {
    val b1 = ball(s1, x, y, c1)
    val ct = ctext(t, x+40, y)
    val b2 = ball(s2, x+80, y, c2)
    b1+ct+b2
  }
  def renderPath(board: Board, c: Color, elems: List[Int]) = {
    val coords = elems map { e => (40+80*board.spaces(e).x) -> (40+80*board.spaces(e).y) }
    path(coords, c.rgb)
  }
  def render(board: Board, cons: List[Constraint], sol: Map[String,Int]) = {
    val paths = board.paths.map map { p: Pair[Color,List[Int]] => renderPath(board, p._1, p._2) } mkString "\n"
    val balls = board.spaces map { s => ball(s.number.toString, 40+80*s.x, 40+80*s.y, s.color.rgb) } mkString "\n"
    val xmax = (board.spaces map { s => (s.x+2)*80 }).max
    val cs = (0 to cons.length-1).toList map { i => renderConstraint(xmax+(i/5)*300, 40+100*(i % 5), cons(i)) }
    val ss = "<!-- "+sol+" -->"
    file(paths+balls+cs+ss)
  }
  def renderConstraint(x: Int, y: Int, c: Constraint) = c match {
    case p: PrimaryConstraint => p match {
      case in: IsNumber => {
        val b1 = ball(p.v, x, y, "#ffffff");
        val eq = ctext("=", x+40, y);
        val b2 = ball(in.number.toString, x+80, y, "#ffffff")
        s""" ${b1} ${eq}, ${b2}"""
      }
      case ic: IsColor => ball(p.v, x, y, ic.color.rgb);
      case iop: IsOnPath => {
        val pa = path(List(x-40 -> y, x+40 -> y), iop.color.rgb);
        val b = ball(p.v, x, y, "#ffffff")
        pa + b
      }
      case ir: IsRow => {
        val r = row(x, y, 0);
        val b = ball(p.v, x, y, "#ffffff")
        val e = ctext("=", x+60, y);
        val t = ctext((ir.row+1).toString, x+80, y);
        r + b + e + t
      }
      case ic: IsColumn => {
        val c = column(x, y, 0);
        val b = ball(p.v, x, y, "#ffffff")
        val e = ctext("=", x+60, y);
        val t = ctext((ic.column+1).toString, x+80, y);
        c + b + e + t
      }
      case _ => s""" <text fill="black" x="$x" y="${y+10}" font-size="32px"  style="text-anchor: left;">$p</text> """
    }
    case s: SecondaryConstraint => s match {
      case GreaterThan(a, b) => renderTwoBallsWithText(s.v1, s.v2, ">", x, y)
      case LessThan(a, b) => renderTwoBallsWithText(s.v1, s.v2, "<", x, y)
      case SecondaryNot(AdjacentTo(a, b)) => renderTwoBallsWithText(s.v1, s.v2, "/", x, y)
      case SameNumber(a, b) => renderTwoBallsWithText(s.v1, s.v2, "=", x, y)
      case AdjacentTo(a, b) => {
        val b1 = ball(s.v1, x, y, "#ffffff")
        val b2 = ball(s.v2, x+80, y, "#ffffff")
        b1+b2
      }
      case SamePath(a, b) => {
        val p1 = path(List(x-40 -> y, x+120 -> y), "#ffffff")
        val b1 = ball(s.v1, x, y, "#ffffff")
        val b2 = ball(s.v2, x+80, y, "#ffffff")
        p1+b1+b2
      }
      case SecondaryNot(SamePath(a, b)) => {
        val p1 = path(List(x-40 -> y, x+120 -> y), "#ffffff")
        p1+renderTwoBallsWithText(s.v1, s.v2, "/", x, y)
      }
      case SameColor(a, b) => renderTwoBallsWithText(s.v1, s.v2, "", x, y, "#808080", "#808080")
      case SecondaryNot(SameColor(a, b)) => renderTwoBallsWithText(s.v1, s.v2, "/", x, y, "#808080", "#808080")
      case SecondaryNot(SameRow(a, b)) => {
        val r = row(x, y, 80);
        val ab = ball(a, x, y, "#ffffff")
        val sl = ctext("/", x+40, y)
        val bb = ball(b, x+80, y, "#ffffff")
        r + ab + sl + bb
      }
      case SecondaryNot(SameColumn(a, b)) => {
        val c = column(x, y, 80);
        val ab = ball(a, x, y, "#ffffff")
        val sl = ctext("/", x+40, y)
        val bb = ball(b, x+80, y, "#ffffff")
        c + ab + sl + bb
      }
      case SameRow(a, b) => {
        val r = row(x, y, 80);
        val ab = ball(a, x, y, "#ffffff")
        val bb = ball(b, x+80, y, "#ffffff")
        r + ab + bb
      }
      case SameColumn(a, b) => {
        val c = column(x, y, 80);
        val ab = ball(a, x, y, "#ffffff")
        val bb = ball(b, x+80, y, "#ffffff")
        c + ab + bb
      }
      case _ => ltext(s.toString, x-12, y+10)
    }
  }
}

