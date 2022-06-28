package itc

import Expr.*
import Command.*
import scala.util.parsing.combinator.RegexParsers

case class ParsingError(msg: String) extends Exception

object Parser extends RegexParsers:
  def error(msg: String): Nothing = throw ParsingError(msg)

  private def wrapR[T](e: => Parser[T]): Parser[T] = "(" ~> e <~ ")"
  private def wrapS[T](e: => Parser[T]): Parser[T] = "[" ~> e <~ "]"

  lazy val keywords = Set(
    "true",
    "false",
    "fun",
    "Int", "Boolean", "Type", "forall", "Prop"
  )
  private lazy val n: Parser[BigInt] = "-?[0-9]+".r ^^ BigInt.apply
  private lazy val b: Parser[Boolean] = "true" ^^^ true | "false" ^^^ false
  private lazy val x: Parser[String] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))

  private lazy val etype: Parser[Type.type] = "Type" ^^^ Type
  private lazy val eprop: Parser[Prop.type] = "Prop" ^^^ Prop
  private lazy val eid: Parser[Id] = x ^^ Id.apply
  private lazy val eint: Parser[Int] = n ^^ Int.apply
  private lazy val ebool: Parser[Bool] = b ^^ Bool.apply
  private lazy val efun: Parser[Expr] =
    (("fun" ~> rep1(wrapR((x <~ ":") ~ eparser)) <~ "=>") ~ eparser) ^^ {
      case nts ~ b => nts.foldRight(b){ case ((name ~ ty), b) => Fun(name, ty, b) }
    }

  private lazy val eintT: Parser[IntT.type] = "Int" ^^^ IntT
  private lazy val ebooleanT: Parser[BooleanT.type] = "Boolean" ^^^ BooleanT
  private lazy val euniv: Parser[Expr] =
    ("forall" ~> rep1(wrapR((x <~ ":") ~ eparser)) <~ ",") ~ eparser ^^ {
    case nts ~ t => nts.foldRight(t) { case ((name ~ ty), t) => Univ(name, ty, t) }
  }

  private lazy val e0: Parser[Expr] =
    eid |
      eint |
      ebool |
      eintT |
      ebooleanT |
      efun |
      euniv |
      etype |
      eprop |
      wrapR(eparser)

  private lazy val aa: Parser[AppArg] =
    e0 ^^ AppArg.E.apply

  enum AppArg:
    case E(e: Expr) extends AppArg

  private lazy val e1: Parser[Expr] =
    (e0 ~ rep(aa)) ^^ { case (eb ~ as) =>
      as.foldLeft(eb) {
        case (e, AppArg.E(e2)) => App(e, e2)
      }
    }

  private lazy val e2: Parser[Expr] =
    rep1sep(e1, "->") ^^ { case (es) =>
      es.reduceRight{
        case (e1, e2) => Univ(TypeCheck.fresh(e2.frees, "x"), e1, e2)
      }
  }

  lazy val eparser: Parser[Expr] = e2


  private lazy val clet: Parser[Let] =
    ("Definition" ~> x <~ ":=") ~ (eparser <~ ".") ^^ { case (s ~ e) =>
      Let(s, e)
    }
  private lazy val cdefine: Parser[Definition] =
    ("Definition" ~> x <~ ":") ~ (eparser <~ ".") ^^ { case (s ~ ty) =>
      Definition(s, ty)
    }
  private lazy val cprint: Parser[Print] = ("Print" ~> x <~ ".") ^^ Print.apply

  lazy val cparser: Parser[Command] = clet | cdefine | cprint

  private lazy val tacintro: Parser[Tactic.Intro] =
    ("intro" ~> x <~ ".") ^^ Tactic.Intro.apply
  private lazy val tacapply: Parser[Tactic.Apply] =
    ("apply" ~> eparser <~ ".") ^^ Tactic.Apply.apply
  private lazy val tacunfold: Parser[Tactic.Unfold] =
    ("unfold" ~> x) ~ opt("in" ~> x) <~ "." ^^ { case x ~ h => Tactic.Unfold(x, h)}
  private lazy val taccbv: Parser[Tactic.Cbv] =
    "cbv" ~> opt("in" ~> x) <~ "." ^^ Tactic.Cbv.apply
  private lazy val taccbn: Parser[Tactic.Cbn] =
    "cbn" ~> opt("in" ~> x) <~ "." ^^ Tactic.Cbn.apply
  private lazy val tacdefined: Parser[Tactic.Defined.type] =
    "Defined" <~ "." ^^^ Tactic.Defined

  lazy val tacparser: Parser[Tactic] = tacintro | tacapply | tacunfold | taccbv | taccbn | tacdefined
end Parser
