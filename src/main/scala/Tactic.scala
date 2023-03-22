package itc

enum Tactic:
  case Intro(x: String) extends Tactic
  case Apply(e: Expr) extends Tactic
  case Unfold(x: String, h: Option[String]) extends Tactic
  case Cbv(h: Option[String]) extends Tactic
  case Cbn(h: Option[String]) extends Tactic
  case Defined extends Tactic
end Tactic
object Tactic:
  def apply(str: String): Tactic = Parser
    .parseAll(Parser.tacparser, str)
    .getOrElse(Parser.error("Tactic parse fail"))
  def manipulate(
      t: Tactic,
      gTenv: Map[String, Expr],
      gEnv: Map[String, Expr],
      ctx: Context,
      he: HoleExpr
  ): Option[(List[Context], HoleExpr)] = t match {
    case Intro(x) =>
      ctx.target._2 match {
        case Expr.Univ(y, k, b) =>
          val newb = b.alpha(y, x)
          Some(
            List(Context(ctx.tenv + (x -> k), (ctx.target._1, newb))),
            he.replace(
              ctx.target._1,
              HoleExpr.Fun(x, k, HoleExpr.Hole(ctx.target._1, newb))
            )
          )
        case _ => { println(s"${ctx.target._2} is not Univ"); None }
      }
    case Apply(e) =>
      TypeCheck(gTenv ++ ctx.tenv.toMap, gEnv, e) match {
        case Some(ty) =>
            def aux(tyctx: Expr, tyapply: Expr): Option[List[Expr]] =
                if (tyctx == tyapply) Some(Nil)
                else tyapply match {
                    case Expr.Univ(p, k, r) => aux(tyctx, r).map((l) => k :: l)
                    case _ => None
                }
            aux(ctx.target._2, ty) match
                case None => {println(s"$e is not applicable"); None }
                case Some(ts) =>
                    val tnums = if (ts.length == 0) Nil else (ctx.target._1 :: he.freshTargets(ts.length - 1))
                    val targets = tnums zip ts
                    val contexts = targets.map((v) => Context(ctx.tenv, v))
                    val nhole = he.replace(ctx.target._1, targets.foldLeft(e.toHole){ case (e, (n, t)) => HoleExpr.App(e, HoleExpr.Hole(n, t))})
                    Some((contexts, nhole))
        case None     => { println(s"$e does not type checks"); None }
      }
    case Unfold(x, h) =>
      gEnv.lift(x) match {
        case Some(t) => h match {
          case Some(h1) => ctx.tenv.lift(h1) match {
            case Some(th) => Some((List(ctx.copy(tenv = ctx.tenv.updated(h1, TypeCheck.subst(th, x, t)))), he))
            case None => None
          }
          case None => Some((List(ctx.copy(target = (ctx.target._1, TypeCheck.subst(ctx.target._2, x, t)))), he))
        }
        case None => None
      }
    case Cbv(h) =>
      h match {
        case Some(h1) => ctx.tenv.lift(h1) match {
            case Some(th) => Some((List(ctx.copy(tenv = ctx.tenv.updated(h1, TypeCheck.interp(Map(), th)))), he))
            case None => None
          }
          case None => Some((List(ctx.copy(target = (ctx.target._1, TypeCheck.interp(Map(), ctx.target._2)))), he))
        }
    case Cbn(h) =>
      h match {
        case Some(h1) => ctx.tenv.lift(h1) match {
            case Some(th) => Some((List(ctx.copy(tenv = ctx.tenv.updated(h1, TypeCheck.cbn(th)))), he))
            case None => None
          }
          case None => Some((List(ctx.copy(target = (ctx.target._1, TypeCheck.cbn(ctx.target._2)))), he))
        }

    case Defined => None
  }
end Tactic
