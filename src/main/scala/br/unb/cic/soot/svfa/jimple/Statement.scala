package br.unb.cic.soot.svfa.jimple

import soot._

import soot._

abstract class Statement(val base: Unit){}

case class AssignStmt(b: Unit) extends Statement(b) {
  val stmt = base.asInstanceOf[soot.jimple.AssignStmt]
}
case class InvokeStmt(b: Unit) extends Statement(b) {
  val stmt = base.asInstanceOf[soot.jimple.InvokeStmt]
}

case class SootIfStmt(b: soot.Unit) extends Statement(b) {
  val stmt = base.asInstanceOf[soot.jimple.IfStmt]
}

case class SootReturnStmt(b: soot.Unit) extends Statement(b) {
  val stmt = base.asInstanceOf[soot.jimple.ReturnStmt]
}

case class InvalidStmt(b: Unit) extends Statement(b)

object Statement {
  def convert(base: Unit): Statement =
    if(base.isInstanceOf[soot.jimple.AssignStmt]) {
      AssignStmt(base)
    }
    else if(base.isInstanceOf[soot.jimple.InvokeStmt]) {
      InvokeStmt(base)
    }
    else if(base.isInstanceOf[soot.jimple.IfStmt]) {
      SootIfStmt(base)
    }
    else if(base.isInstanceOf[soot.jimple.ReturnStmt]) {
      SootReturnStmt(base)
    }
    else InvalidStmt(base)
}
