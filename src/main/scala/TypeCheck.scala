package itc

object TypeCheck:
  def apply(tenv: Map[String, Type], e: Expr): Option[Type] = e match
    case Expr.Int(_)  => Some(Type.IntT)
    case Expr.Bool(_) => Some(Type.BooleanT)
    case Expr.Fun(x, t, e) =>
      apply(tenv + (x -> t), e) match
        case Some(a) => Some(Type.ArrowT(t, a))
        case None    => None
    case Expr.App(e1, e2) =>
      apply(tenv, e1) match
        case Some(Type.ArrowT(p, r)) =>
          apply(tenv, e2) match
            case Some(a) => if (equiv(p, a)) Some(r) else None
            case _       => None
        case _ => None
    case Expr.Id(x) => tenv.lift(x)
    case Expr.TApp(e1, t) =>
      apply(tenv, e1) match
        case Some(Type.FunT(x, tb)) => Some(subst(tb, x, t))
        case _                      => None
    case Expr.TFun(x, b) =>
      apply(tenv + (x -> Type.IdT(x)), b) match
        case Some(a) => Some(Type.FunT(x, a))
        case None    => None

  def fresh(s: Set[String]): String =
    def aux(n: Int): String =
      if (!(s contains s"a$n")) s"a$n" else aux(n+1)
    aux(0)

  def equiv(t1: Type, t2: Type): Boolean = (t1, t2) match
    case (Type.FunT(s1, b1), Type.FunT(s2, b2)) =>
      val s3 = fresh(b1.frees ++ b2.frees)
      equiv(b1.alpha(s1, s3), b2.alpha(s2, s3))
    case (Type.IdT(s1), Type.IdT(s2)) => s1 == s2
    case (Type.ArrowT(p1, r1), Type.ArrowT(p2, r2)) => equiv(p1, p2) && equiv(r1, r2)
    case (Type.IntT, Type.IntT) => true
    case (Type.BooleanT, Type.BooleanT) => true
    case _ => false

  def subst(tb: Type, x: String, t: Type): Type = tb match
    case Type.FunT(s, b) => if s == x then tb else Type.FunT(s, subst(b, x, t))
    case Type.IdT(s) => if s == x then t else tb
    case Type.ArrowT(p, r) => Type.ArrowT(subst(p, x, t), subst(r, x, t))
    case Type.IntT => Type.IntT
    case Type.BooleanT => Type.BooleanT

end TypeCheck