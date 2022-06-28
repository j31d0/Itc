package itc

object TypeCheck:
  def apply(kenv: Map[String, Kind], tenv: Map[String, Type], e: Expr): Option[Type] = e match
    case Expr.Int(_)  => Some(Type.IntT)
    case Expr.Bool(_) => Some(Type.BooleanT)
    case Expr.Fun(x, t, e) =>
      KindCheck.apply(kenv, t) match
        case Some(Kind.ProperK) =>
          apply(kenv, tenv + (x -> t), e) match
          case Some(a) => Some(Type.ArrowT(t, a))
          case None    => None
        case _ => None
    case Expr.App(e1, e2) =>
      apply(kenv, tenv, e1) match
        case Some(Type.ArrowT(p, r)) =>
          apply(kenv, tenv, e2) match
            case Some(a) => if (equiv(p, a)) Some(r) else None
            case _       => None
        case _ => None
    case Expr.Id(x) => tenv.lift(x)
    case Expr.TApp(e1, t) =>
      apply(kenv, tenv, e1) match
        case Some(Type.UnivT(x, k, tb)) => Some(subst(tb, x, t))
        case _                          => None
    case Expr.TFun(x, k, b) =>
      apply(kenv + (x -> k), tenv, b) match
        case Some(a) => Some(Type.UnivT(x, k, a))
        case None    => None

  def fresh(s: Set[String]): String =
    def aux(n: Int): String =
      if (!(s contains s"a$n")) s"a$n" else aux(n+1)
    aux(0)

  def equiv(t1: Type, t2: Type): Boolean = (t1, t2) match
    case (Type.FunT(s1, _, b1), Type.FunT(s2, _, b2)) =>
      val s3 = fresh(b1.frees ++ b2.frees)
      equiv(b1.alpha(s1, s3), b2.alpha(s2, s3))
    case (Type.UnivT(s1, _, b1), Type.UnivT(s2, _, b2)) =>
      val s3 = fresh(b1.frees ++ b2.frees)
      equiv(b1.alpha(s1, s3), b2.alpha(s2, s3))
    case (Type.IdT(s1), Type.IdT(s2)) => s1 == s2
    case (Type.ArrowT(p1, r1), Type.ArrowT(p2, r2)) => equiv(p1, p2) && equiv(r1, r2)
    case (Type.IntT, Type.IntT) => true
    case (Type.BooleanT, Type.BooleanT) => true
    case _ => false

  def subst(tb: Type, x: String, t: Type): Type = tb match
    case Type.AppT(t1, t2) => Type.AppT(subst(t1, x, t), subst(t2, x, t))
    case Type.FunT(s, k, b) => if s == x then tb else
      val s3 = fresh(b.frees ++ t.frees + s)
      Type.FunT(s3, k, subst(b.alpha(s, s3), x, t))
    case Type.UnivT(s, k, b) => if s == x then tb else
      val s3 = fresh(b.frees ++ t.frees + s)
      Type.UnivT(s3, k, subst(b.alpha(s, s3), x, t))
    case Type.IdT(s) => if s == x then t else tb
    case Type.ArrowT(p, r) => Type.ArrowT(subst(p, x, t), subst(r, x, t))
    case Type.IntT => Type.IntT
    case Type.BooleanT => Type.BooleanT


  def tinterp(tb: Type): Type = tb match
    case Type.AppT(t1, t2) =>
      val t1a = tinterp(t1)
      t1a match
        case Type.FunT(s, k, b) => tinterp(subst(b, s, t2))
        case _ => Type.AppT(t1a, t2)
    case _ => tb

end TypeCheck