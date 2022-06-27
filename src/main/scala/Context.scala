package itc

case class Context(kenv: Map[String, Kind], tenv: Map[String, Type], target: (Int, Type)) {
    def toDetail: String =
        "\n" +
        s"goal ${target._1}\n\n" +
        kenv.map{ case (s, t) => s" ${s.toString} : ${t.toString}\n"}.mkString +
        tenv.map{ case (s, t) => s" ${s.toString} : ${t.toString}\n"}.mkString +
        "\n ===============================\n" +
        s" ${target._2}\n"
    
    def toShort: String = s" goal ${target._1}: ${target._2}"
}
