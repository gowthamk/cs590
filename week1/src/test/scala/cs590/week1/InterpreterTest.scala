package cs590.week1

import org.scalatest.FunSuite

class InterpreterTest extends FunSuite with Interpreter1 {

  test("ite") {
    val test = Eq(Var("n"),Const(0))
    val texp = Const(1)
    val fexp = Plus(Var("n"),Times(Var("n"),Const(2)))
    val lam = Lam("n",Cond(test,texp,fexp))
    val app = App(lam,Const(2))
    val emptyEnv = (i:Ident) => throw new Exception
    assert(eval(app,emptyEnv).asInstanceOf[Int] == 6)
  }

  test("fact") {
  /**
   * TODO: interpret factorial function
   */
    val test = Eq(Var("n"),Const(0))
    val texp = Const(1)
    val app = App(Var("fact"), Plus(Var("n"),Const(-1)))
    val fexp = Times(Var("n"),app)
    val dlam = Lam("n", Cond(test,texp,fexp))
    val body = App(Var("fact"), Const(3))
    val letrec = Letrec("fact", dlam, body)
    val emptyEnv = (i:Ident) => throw new Exception
    assert(eval(letrec,emptyEnv).asInstanceOf[Int] == 6)
  }

}

class InterpreterTest2 extends FunSuite with Interpreter2 {

  test("ite2") {
    val test = Eq(Var("n"),Const(0))
    val texp = Const(1)
    val fexp = Plus(Var("n"),Times(Var("n"),Const(2)))
    val lam = Lam("n",Cond(test,texp,fexp))
    val app = App(lam,Const(2))
    assert(eval(app,Init()).asInstanceOf[Int] == 6)
  }

  test("fact2") {
  /**
   * TODO: interpret factorial function
   */
    val test = Eq(Var("n"),Const(0))
    val texp = Const(1)
    val app = App(Var("fact"), Plus(Var("n"),Const(-1)))
    val fexp = Times(Var("n"),app)
    val dlam = Lam("n", Cond(test,texp,fexp))
    val body = App(Var("fact"), Const(3))
    val letrec = Letrec("fact", dlam, body)
    assert(eval(letrec,Init()).asInstanceOf[Int] == 6)
  }

}
