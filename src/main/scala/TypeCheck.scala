package itc

object TypeCheck:
    def apply(tenv: Map[String, Type], e: Expr): Option[Type] = e match
        case Expr.Int(_) => Some(Type.IntT)
        case Expr.Bool(_) => Some(Type.BooleanT)
        case Expr.Fun(x, t, e) => apply(tenv + (x -> t), e) match
            case Some(a) => Some(Type.ArrowT(t, a))
            case None => None
        case Expr.App(e1, e2) => apply(tenv, e1) match
            case Some(Type.ArrowT(p, r)) => apply(tenv, e2) match
                case Some(a) => if (r == a) Some(a) else None
                case _ => None
            case _ => None
        case Expr.Id(x) => tenv.lift(x)

end TypeCheck