package com.pourtois.symbolette

import com.pourtois.symbolette.expr.Expr._
/**
  * Created by manpo on 17/10/2016.
  */
class AxiSym {
  val t = symbol("t")
  val r = symbol("r")
  val z = symbol("z")
  val phi = symbol("phi")

  val psi = symbol("psi", r::z::Nil)
  val gamma = symbol("gamma", r::z::Nil)



}
