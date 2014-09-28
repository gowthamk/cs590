package cs590.week1

/**
 * Problem 2: Definitional Interpreters
 */


/**
 * Interpreter I from Reynolds' paper
 */
trait Interpreter1 {

  type Val = Any // Int, Boolean, Val => Val
  type Fun = Val => Val

  type Ident = String

  type Env = Ident => Val

  abstract class Exp
  case class Const(x: Val) extends Exp
  case class Var(s: Ident) extends Exp
  case class App(opr: Exp, opnd: Exp) extends Exp
  case class Lam(fp: Ident, body: Exp) extends Exp
  /* New Stuff */
  case class Cond(test: Exp, t: Exp, f: Exp) extends Exp
  case class Letrec(dvar: Ident, dexp: Lam, body: Exp) extends Exp
  case class Plus(a: Exp, b: Exp) extends Exp
  case class Times(a: Exp, b: Exp) extends Exp
  case class Eq(a:Exp, b:Exp) extends Exp

  def eval(e: Exp, env: Env): Val = e match {
    case Const(x) => x
    case Var(x)   => env(x)
    case App(f,x) => eval(f,env).asInstanceOf[Fun](eval(x,env))
    case Lam(x,e) => evlambda(x,e,env)
    case Cond(test,t,f) => (eval(test,env)) match {
      case (x: Boolean) => if (x) eval(t,env) else eval(f,env)
    }
    /**
     * Letrec: The trick is to encode the closure trapping recursive
     * reference in embedded language as a closure trapping recursive 
     * reference in Scala.
     * Note: This could be different from Reynolds' approach.
     */
    case Letrec(dvar,dlam,body) => {
      def rfun (arg:Val) : Val = {
        val clos = eval(dlam, ext(dvar, (v:Val) => rfun(v), env))
        clos.asInstanceOf[Val => Val](arg)
      }
      eval(body,ext(dvar, (v:Val) => rfun(v), env))
    }
    case Plus(a,b) => eval(a,env).asInstanceOf[Int] + 
      eval(b,env).asInstanceOf[Int]
    case Times(a,b) => eval(a,env).asInstanceOf[Int] *
      eval(b,env).asInstanceOf[Int]
    case Eq(a,b) => eval(a,env) == eval(b,env)
  }

  def evlambda(x: Ident, e: Exp, env: Env) = (a: Val) => eval(e,ext(x,a,env))
  def ext(z: Ident, a: Val, env: Env): Env = 
    (i: Ident) => if (i==z) a else env(i)

  /**
   * TODO: what is missing? add it in!
   */

}


/**
 * Interpreter II from Reynolds' paper
 */

trait Interpreter2 {

  type Ident = String

  type Val = Any // Int, Boolean, Val => Val

  abstract class Fun
  case class Clos(lam: Lam, env: Env) extends Fun
  case class SC() extends Fun
  case class EQ1() extends Fun
  case class EQ2(arg1: Val) extends Fun

  abstract class Env
  case class Init() extends Env
  case class Simp(bvar: Ident, bval: Val, old: Env) extends Env
  case class Rec(letrec: Letrec, old: Env) extends Env

  abstract class Exp
  case class Const(x: Val) extends Exp
  case class Var(s: Ident) extends Exp
  case class App(opr: Exp, opnd: Exp) extends Exp
  case class Lam(fp: Ident, body: Exp) extends Exp
  /* New stuff */
  case class Cond(test: Exp, t: Exp, f: Exp) extends Exp
  case class Letrec(dvar: Ident, dexp: Lam, body: Exp) extends Exp
  case class Plus(a: Exp, b: Exp) extends Exp
  case class Times(a: Exp, b: Exp) extends Exp
  case class Eq(a:Exp, b:Exp) extends Exp

  /**
   * TODO: implement second interpreter from paper
   */

  def interpret(r: Exp) = eval(r, Init())

  def eval(r: Exp, env: Env): Val = r match {
    case Const(x) => x
    case Var(x)   => get(env,x)
    case App(f,x) => apply(eval(f,env).asInstanceOf[Fun], eval(x,env))
    case Lam(_,_) => Clos(r.asInstanceOf[Lam],env)
    case Cond(test,t,f) => (eval(test,env)) match {
      case (x: Boolean) => if (x) eval(t,env) else eval(f,env)
    }
    case Letrec(dvar, dexp, body) => {
      val newenv = extrec(env, r.asInstanceOf[Letrec])
      eval(body, newenv)
    }
    case Plus(a,b) => eval(a,env).asInstanceOf[Int] + 
      eval(b,env).asInstanceOf[Int]
    case Times(a,b) => eval(a,env).asInstanceOf[Int] *
      eval(b,env).asInstanceOf[Int]
    case Eq(a,b) => eval(a,env) == eval(b,env)
  }

  def apply(f: Fun, a: Val): Val = f match {
    case Clos(lam: Lam,env) => lam match {
      case Lam(fp,body) => eval(body,ext(env,fp,a))
    }
    case SC() => a.asInstanceOf[Int] + 1
    case EQ1() => EQ2(a)
    case EQ2(arg1) => arg1 == a 
  }

  def ext(env: Env, x: Ident, v: Val): Env = Simp(x,v,env)
  def extrec(env: Env, letrec:Letrec): Env = Rec(letrec,env)

  def get(env: Env, x: Ident): Val = env match {
    case Init() => throw new Exception
    case Simp(bvar,bval,oldenv) => if (x==bvar) bval else get(oldenv,x)
    /*
     * Whenever a letrec var is looked up, the result is a new
     * closure.
     */
    case Rec(Letrec(dvar,dexp,body),oldenv) => 
      /*
       * Looks like dynamic scope, but not.
       */
      if (x==dvar) Clos(dexp, env) 
      else get(oldenv,x)
  }

}


