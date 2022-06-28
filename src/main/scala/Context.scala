package itc

case class Context(tenv: Map[String, Expr], target: (Int, Expr)) {
    def toDetail: String =
        "\n" +
        s"goal ${target._1}\n\n" +
        tenv.map{ case (s, t) => s" ${s.toString} : ${t.toString}\n"}.mkString +
        "\n ===============================\n" +
        s" ${target._2}\n"
    
    def toShort: String = s" goal ${target._1}: ${target._2}"
}
