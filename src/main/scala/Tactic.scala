package itc

enum Tactic:
  case Intro(x: String) extends Tactic
  case Apply(e: Expr) extends Tactic
  case Unfold(x: String, h: Option[String]) extends Tactic
  case Defined extends Tactic
end Tactic
object Tactic:
  def apply(str: String): Tactic = Parser
    .parseAll(Parser.tacparser, str)
    .getOrElse(Parser.error("Tactic parse fail"))
  def manipulate(
      t: Tactic,
      gTalias: Map[String, Type],
      gTenv: Map[String, Type],
      ctx: Context,
      he: HoleExpr
  ): Option[(List[Context], HoleExpr)] = t match {
    case Intro(x) =>
      ctx.target._2 match {
        case Type.ArrowT(p, r) =>
          Some(
            List(Context(ctx.kenv, ctx.tenv + (x -> p), (ctx.target._1, r))),
            he.replace(
              ctx.target._1,
              HoleExpr.Fun(x, p, HoleExpr.Hole(ctx.target._1, r))
            )
          )
        case Type.FunT(y, b) =>
          val newb = b.alpha(y, x)
          Some(
            List(Context(ctx.kenv + (x -> Kind.DotKind), ctx.tenv, (ctx.target._1, newb))),
            he.replace(
              ctx.target._1,
              HoleExpr.TFun(x, HoleExpr.Hole(ctx.target._1, newb))
            )
          )
        case _ => { println(s"${ctx.target._2} is not arrowT"); None }
      }
    case Apply(e) =>
      TypeCheck(gTenv ++ ctx.tenv.toMap, e) match {
        case Some(ty) =>
            def aux(tyctx: Type, tyapply: Type): Option[List[Type]] =
                if (tyctx == tyapply) Some(Nil)
                else tyapply match {
                    case Type.ArrowT(p, r) => aux(tyctx, r).map((l) => p :: l)
                    case _ => None
                }
            aux(ctx.target._2, ty) match
                case None => {println(s"$e is not applicable"); None }
                case Some(ts) =>
                    val tnums = if (ts.length == 0) Nil else (ctx.target._1 :: he.freshes(ts.length - 1))
                    val targets = tnums zip ts
                    val contexts = targets.map((v) => Context(ctx.kenv, ctx.tenv, v))
                    val nhole = he.replace(ctx.target._1, targets.foldLeft(e.toHole){ case (e, (n, t)) => HoleExpr.App(e, HoleExpr.Hole(n, t))})
                    Some((contexts, nhole))
        case None     => { println(s"$e does not type checks"); None }
      }
    case Unfold(x, h) =>
      gTalias.lift(x) match {
        case Some(t) => h match {
          case Some(h1) => ctx.tenv.lift(h1) match {
            case Some(th) => Some((List(ctx.copy(tenv = ctx.tenv.updated(h1, TypeCheck.subst(th, x, t)))), he))
            case None => None
          }
          case None => Some((List(ctx.copy(target = (ctx.target._1, TypeCheck.subst(ctx.target._2, x, t)))), he))
        }
        case None => None
      }
    case Defined => None
  }
end Tactic
