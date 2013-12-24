package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/23/13.
 */
object SVGRender {
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
  def path(points: List[(Int,Int)], color: String) = {
    val path = points map { p => s"${p._1},${p._2}"} mkString " "
    s"""
    <polyline points="$path" style="fill:none;stroke:black;stroke-width:12" />
    <polyline points="$path" style="fill:none;stroke:$color;stroke-width:10" />
    """
  }
  def ball(num: String, x: Int, y: Int, color: String) = {
    s"""
      <circle cx="$x" cy="$y" r="25" stroke="black" stroke-width="1" fill="$color" />
      <text fill="black" x="$x" y="${y+10}" font-size="32px"  style="text-anchor: middle;">$num</text>
    """
  }
  def ctext(s: String, x: Int, y: Int) = {
    s""" <text fill="black" x="$x" y="${y+10}" font-size="32px"  style="text-anchor: middle;">$s</text> """
  }
  def ltext(s: String, x: Int, y: Int) = {
    s""" <text fill="black" x="$x" y="${y+10}" font-size="32px"  style="text-anchor: left;">$s</text> """
  }
  def renderTwoBallsWithText(s1: String, s2: String, t: String, x: Int, y: Int) = {
    val b1 = ball(s1, x, y, "#ffffff")
    val ct = ctext(t, x+40, y)
    val b2 = ball(s2, x+80, y, "#ffffff")
    b1+ct+b2
  }
  def renderPath(board: Board, c: Color, elems: List[Int]) = {
    val coords = elems map { e => ((40+80*board.spaces(e).x) -> (40+80*board.spaces(e).y)) }
    path(coords, c.rgb)
  }
  def render(board: Board, cons: List[SimdConstraint], sol: Map[String,Int]) = {
    val paths = board.paths.map map { p: Pair[Color,List[Int]] => renderPath(board, p._1, p._2) } mkString "\n"
    val balls = board.spaces map { s => ball(s.number.toString, 40+80*s.x, 40+80*s.y, s.color.rgb) } mkString "\n"
    val cs = (0 to cons.length-1).toList map { i => renderConstraint(500, 40+80*i, cons(i)) }
    file(paths+balls+cs)
  }
  def renderConstraint(x: Int, y: Int, c: SimdConstraint) = c match {
    case p: PrimaryConstraint => p match {
      case in: IsNumber => {
        val b1 = ball(p.ball, x, y, "#ffffff");
        val eq = ctext("=", x+40, y);
        val b2 = ball(in.n.toString, x+80, y, "#ffffff")
        s""" ${b1} ${eq}, ${b2}"""
      }
      case ic: IsColor => ball(p.ball, x, y, ic.c.rgb);
      case iop: IsOnPath => {
        val pa = path(List(x-40 -> y, x+40 -> y), iop.c.rgb);
        val b = ball(p.ball, x, y, "#ffffff")
        pa + b
      }
      case ir: IsRow => {
        val b = ball(p.ball, x, y, "#ffffff")
        val t = ltext("is in row #"+(ir.n+1), x+40, y);
        b + t
      }
      case ic: IsColumn => {
        val b = ball(p.ball, x, y, "#ffffff")
        val t = ltext("is in column #"+(ic.n+1), x+40, y);
        b + t
      }
      case _ => s""" <text fill="black" x="$x" y="${y+10}" font-size="32px"  style="text-anchor: left;">$p</text> """
    }
    case s: SecondaryConstraint => s match {
      case GreaterThan(a, b) => renderTwoBallsWithText(s.b1, s.b2, ">", x, y)
      case LessThan(a, b) => renderTwoBallsWithText(s.b1, s.b2, "<", x, y)
      case NotAdjacentTo(a, b) => renderTwoBallsWithText(s.b1, s.b2, "/", x, y)
      case SameNumber(a, b) => renderTwoBallsWithText(s.b1, s.b2, "=", x, y)
      case AdjacentTo(a, b) => {
        val b1 = ball(s.b1, x, y, "#ffffff")
        val b2 = ball(s.b2, x+80, y, "#ffffff")
        b1+b2
      }
      case IsOnPathWith(a, b) => {
        val p1 = path(List(x-40 -> y, x+120 -> y), "#ffffff")
        val b1 = ball(s.b1, x, y, "#ffffff")
        val b2 = ball(s.b2, x+80, y, "#ffffff")
        p1+b1+b2
      }
      case IsNotOnPathWith(a, b) => {
        val p1 = path(List(x-40 -> y, x+120 -> y), "#ffffff")
        p1+renderTwoBallsWithText(s.b1, s.b2, "/", x, y)
      }
      case SameColor(a, b) => ltext(a+" is the same color as "+b, x-12, y+10)
      case NotSameColor(a, b) => ltext(a+" is a different color from "+b, x-12, y+10)
      case DifferentRow(a, b) => ltext(a+" is in a different row from "+b, x-12, y+10)
      case DifferentColumn(a, b) => ltext(a+" is in a different column from "+b, x-12, y+10)
      case SameRow(a, b) => ltext(a+" is in the same row as "+b, x-12, y+10)
      case SameColumn(a, b) => ltext(a+" is in the same column as "+b, x-12, y+10)
      case _ => ltext(s.toString, x-12, y+10)
    }
  }
}
