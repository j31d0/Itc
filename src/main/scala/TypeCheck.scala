package itc

object TypeCheck:
  def apply(tenv: Map[String, Expr], e: Expr): Option[Expr] = e match
    case Expr.Type => None
    case Expr.Prop => Some(Expr.Type)
    case Expr.Int(_)  => Some(Expr.IntT)
    case Expr.Bool(_) => Some(Expr.BooleanT)
    case Expr.IntT => Some(Expr.Type)
    case Expr.BooleanT => Some(Expr.Type)
    case Expr.Fun(x, t, e) =>
      TypeCheck.apply(tenv, t) match
        case Some(_) =>
          apply(tenv + (x -> t), e) match
          case Some(b) => Some(Expr.Univ(x, t, b))
          case None    => None
        case _ => None
    case Expr.Univ(x, t, e) =>
      TypeCheck.apply(tenv, t) match
        case Some(_) =>
          apply(tenv + (x -> t), e) match
            case Some(l) => Some(l)
            case None => None
        case _ => None
    case Expr.App(e1, e2) =>
      apply(tenv, e1) match
        case Some(Expr.Univ(x, t, b)) =>
          apply(tenv, e2) match
            case Some(a) => if (equiv(interp(t), a)) Some(interp(subst(b, x, e2))) else None
            case _       => None
        case _ => None
    case Expr.Id(x) => tenv.lift(x)

  def fresh(s: Set[String], prefix: String): String =
    val nprefix = prefix.takeWhile((c) => !c.isDigit)
    def aux(n: Int): String =
      if (!(s contains s"$nprefix$n")) s"$nprefix$n" else aux(n+1)
    aux(0)

  def equiv(t1: Expr, t2: Expr): Boolean = (t1, t2) match
    case (Expr.Fun(s1, _, b1), Expr.Fun(s2, _, b2)) =>
      val s3 = fresh(b1.frees ++ b2.frees, "A")
      equiv(b1.alpha(s1, s3), b2.alpha(s2, s3))
    case (Expr.Univ(s1, _, b1), Expr.Univ(s2, _, b2)) =>
      val s3 = fresh(b1.frees ++ b2.frees, "A")
      equiv(b1.alpha(s1, s3), b2.alpha(s2, s3))
    case (Expr.Id(s1), Expr.Id(s2)) => s1 == s2
    case (Expr.IntT, Expr.IntT) => true
    case (Expr.BooleanT, Expr.BooleanT) => true
    case (Expr.Int(n1), Expr.Int(n2)) => n1 == n2
    case (Expr.Bool(b1), Expr.Bool(b2)) => b1 == b2
    case (Expr.Prop, Expr.Prop) => true
    case (Expr.Type, Expr.Type) => true
    case _ => false

  def subst(tb: Expr, x: String, t: Expr): Expr = tb match
    case Expr.App(t1, t2) => Expr.App(subst(t1, x, t), subst(t2, x, t))
    case Expr.Fun(s, k, b) => if s == x then Expr.Fun(s, subst(k, x, t), b) else
      val avoidSet = k.frees ++ b.bindings ++ b.frees ++ t.frees
      val s3 = if (avoidSet contains s) fresh(k.frees ++ b.bindings ++ b.frees ++ t.frees, s) else s
      Expr.Fun(s3, subst(k, x, t), subst(b.alpha(s, s3), x, t))
    case Expr.Univ(s, k, b) => if s == x then Expr.Univ(s, subst(k, x, t), b) else
      val avoidSet = k.frees ++ b.bindings ++ b.frees ++ t.frees
      val s3 = if (avoidSet contains s) fresh(k.frees ++ b.bindings ++ b.frees ++ t.frees, s) else s
      Expr.Univ(s3, subst(k, x, t), subst(b.alpha(s, s3), x, t))
    case Expr.Id(s) => if s == x then t else tb
    case Expr.IntT | Expr.BooleanT | Expr.Prop | Expr.Type | Expr.Int(_) | Expr.Bool(_) => tb


  def interp(tb: Expr): Expr = tb match
    case Expr.App(t1, t2) =>
      val t1a = interp(t1)
      t1a match
        case Expr.Fun(s, k, b) => interp(subst(b, s, t2))
        case _ => Expr.App(t1a, t2)
    case _ => tb
  
  def cbn (tb: Expr): Expr = tb match
    case Expr.App(t1, t2) =>
      val t1a = cbn(t1)
      t1a match
        case Expr.Fun(s, k, b) => cbn(subst(b, s, t2))
        case _ => Expr.App(t1a, cbn(t2))
    case Expr.Fun(s, k, b) => Expr.Fun(s, cbn(k), cbn(b))
    case Expr.Univ(s, k, b) => Expr.Univ(s, cbn(k), cbn(b))
    case Expr.Id(_) | Expr.IntT | Expr.BooleanT | Expr.Prop | Expr.Type | Expr.Int(_) | Expr.Bool(_) => tb

end TypeCheck