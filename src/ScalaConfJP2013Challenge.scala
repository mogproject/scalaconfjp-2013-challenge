object ScalaConfJP2013Challenge {
  def quiz1(intersections: Array[String]): Int = {
    val initState = intersections.map(_.map(_ == '0').toArray)
    val n = initState.size
    val m = initState(0).size

    def f(x: Int, y: Int, state: Array[Array[Boolean]]): Int = {
      if (x < 0 || x >= n || y < 0 || y >= m || ! state(x)(y)) return 0
      if (x == n - 1 && y == m - 1) return 1
      val nextState = for (i <- state) yield {for (j <- i) yield j}
      nextState(x)(y) = false
      List((0, 1), (1, 0), (0, -1), (-1, 0)).map {
        d => f(x + d._1, y + d._2, nextState)
      }.sum
    }
    f(0, 0, initState)
  }

  def quiz2(chestnuts: Array[String]): Int = {
    val a = chestnuts.map(_.toArray.map(_.toString.toInt))
    def f(i: Int, j: Int): Int = if (i >= 0 && j >= 0) math.max(f(i - 1, j), f(i, j - 1)) + a(i)(j) else 0
    f(a.size - 1, a(0).size - 1)
  }

  def quiz3(rails: Array[(String, String, Int, Int)]): String = {
    def f(edges: Array[(String, String, Int)]): (Int, String) = {
      // Create set of stations.
      val stations = edges.map(_._1).toSet ++ edges.map(_._2).toSet
      val INF = 9999

      // Initialize.
      val m: collection.mutable.Map[(String, String), Int] = collection.mutable.Map()
      val route: collection.mutable.Map[(String, String), List[String] ] = collection.mutable.Map()
      for (i <- stations; j <- stations) {
        m.put((i, j), if (i == j) 0 else INF)
        route.put((i, j), Nil)
      }

      for (i <- edges) m.update((i._1, i._2), i._3)

      // Use Warshall-Floyd Algorithm.
      for (k <- stations; i <- stations; j <- stations) {
        val x: Int = m.getOrElse((i, j), 0)
        val y = m.getOrElse((i, k), 0) + m.getOrElse((k, j), 0)
        if (x > y) {
          m.update((i, j), y)
          route.update((i, j),
            route.getOrElse(
              (i, k), Nil) ::: List(k) ::: route.getOrElse((k, j), Nil))
        }
      }

      val src = "たかし家"
      val dst = "市場前"
      val shortestValue = m.getOrElse((src, dst), 0)
      val shortestRoute = route.getOrElse((src, dst), Nil)
      (shortestValue, (src :: shortestRoute ::: List(dst)).mkString(" -> "))
    }

    val shortest = f(rails.map(i => (i._1, i._2, i._3)))
    val cheapest = f(rails.map(i => (i._1, i._2, i._4)))
    "%s: %d km\n%s: %d yen".format(shortest._2, shortest._1, cheapest._2, cheapest._1)
  }

  def main(args: Array[String]) {
    // Question 1.
    val arg1 = Array(
      "01010",
      "00001",
      "00000",
      "01000",
      "01000"
    )
    println(quiz1(arg1))
    assume(quiz1(arg1) == 80)

    // Question 2.
    val arg2 = Array(
      "05835295",
      "75938274",
      "37564421",
      "05739784",
      "36460638",
      "29686821",
      "18598230",
      "97739214"
    )
    println(quiz2(arg2))
    assume(quiz2(arg2) == 86)

    // Question 3.
    val arg3 = Array(
      ("たかし家", "御湯ノ水", 2, 160),
      ("御湯ノ水", "夏葉原", 15, 140),
      ("御湯ノ水", "霜ヶ関", 21, 160),
      ("夏葉原", "小森", 18, 150),
      ("霜ヶ関", "小森", 14, 140),
      ("霜ヶ関", "器川", 2, 30),
      ("器川", "小森", 13, 120),
      ("器川", "夏葉原", 14, 140),
      ("小森", "市場前", 5, 140)
    )
    println(quiz3(arg3))
    assume(quiz3(arg3) ==
      "たかし家 -> 御湯ノ水 -> 夏葉原 -> 小森 -> 市場前: 40 km\n" +
      "たかし家 -> 御湯ノ水 -> 夏葉原 -> 小森 -> 市場前: 590 yen")
  }
}
