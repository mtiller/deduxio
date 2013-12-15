package com.xogeny.puzzles.tictac

import JaCoP.core.{IntVar, Store}
import JaCoP.constraints._
import java.util
import JaCoP.search._

/**
 * Created by mtiller on 12/14/13.
 */

class TicTac(nrows: Int, ncols: Int) {
  var s = new Store();
  val sv1 = new IntVar(s, "sumvar1", 1, 1);
  val sv2 = new IntVar(s, "sumvar2", 2, 2);
  val rsv = new IntVar(s, "rowsum", ncols/2, ncols/2);
  val csv = new IntVar(s, "colsum", nrows/2, nrows/2);
  val mkVar = { (i: Int, j: Int) => new IntVar(s, ("b"+i)+j, 0, 1); }
  val rows= 1 to nrows;
  val cols = 1 to ncols;
  val coords = rows flatMap { i => cols map { j => (i,j) }}
  val vars = Map() ++ (coords map { p => p -> mkVar(p._1, p._2) })
  val notThree = { (v1: IntVar, v2: IntVar, v3: IntVar) =>
    new Or(Array[PrimitiveConstraint](new XplusYplusQeqZ(v1, v2, v3, sv1),
      new XplusYplusQeqZ(v1, v2, v3, sv2)))}
  val hcon = { (i: Int, j: Int) =>
    val v1 = vars.get((i,j)).get
    val v2 = vars.get((i,j+1)).get
    val v3 = vars.get((i,j+2)).get
    notThree(v1, v2, v3);
  }
  val vcon = { (i: Int, j: Int) =>
    val v1 = vars.get((i,j)).get
    val v2 = vars.get((i+1,j)).get
    val v3 = vars.get((i+2,j)).get
    notThree(v1, v2, v3);
  }
  def setX(i: Int, j: Int) = s.impose(new XeqC(vars.get((i,j)).get, 1))
  def setO(i: Int, j: Int) = s.impose(new XeqC(vars.get((i,j)).get, 0))
  val hcons = rows flatMap { i => cols dropRight 2 map { j => hcon(i, j) }};
  val vcons = rows dropRight 2  flatMap { i => cols map { j => vcon(i, j) }};
  val rvars: Seq[Array[IntVar]] = rows map { i => (cols map { j => vars.get((i,j)).get }).toArray}
  val cvars: Seq[Array[IntVar]] = cols map { j => (rows map { i => vars.get((i,j)).get }).toArray}

  rvars map { r => s.impose(new Sum(r, rsv))}
  cvars map { c => s.impose(new Sum(c, csv))}
  hcons map { c => s.impose(c) }
  vcons map { c => s.impose(c) }
  def solve() = {
    val vs = vars.values.toArray;
    var search: Search[IntVar]  = new DepthFirstSearch[IntVar]();
    var select: SelectChoicePoint[IntVar]  =
      new InputOrderSelect[IntVar](s, vs,
        new IndomainMin[IntVar]());
    val result = search.labeling(s, select);

    if ( result ) {
      System.out.println("Solution: ");
      rows map { i => { cols map { j => print(vars.get((i,j)).get.value())}; println(""); }}
      //vars map { p => println(p._1+" = "+p._2 )}
    } else {
      System.out.println("*** No");
    }
  }
}

object TestRun {
  def main(args: Array[String]): Unit = {
    prob1();
  }
  def prob1(): Unit = {
    val p = new TicTac(6, 6);
    p.setX(1,2)
    p.setX(1,3)
    p.setO(2,6)
    p.setX(3,1)
    p.setO(3,4)
    p.setO(4,2)
    p.setX(5,3)
    p.setO(5,5)
    p.setO(6,5)
    p.solve()
  }
  def prob2(): Unit = {
    val p = new TicTac(6, 6);
    p.setX(1,4)
    p.setO(2,2)
    p.setO(2,3)
    p.setO(3,1)
    p.setX(3,6)
    p.setX(5,2)
    p.setX(5,4)
    p.setX(5,6)
    p.setX(6,1)
    p.setX(6,6)

    p.solve()
  }
}

class Test {
}
