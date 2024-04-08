object Week9 {
  val x = 17
  val y = 5
  val res = {
    var z: Int = 0;
    {
      var t: Int = x;
      while (y <= t) {
        z = z + 1;
        t = t - y
      };
      z
    }
  }

  //  Recursion

  def subY(z: Int, t: Int, y: Int): Int = {
    if (y <= t) {
      subY(z + 1, t - y, y)
    } else {
      z
    }
  }

  def main(args: Array[String]): Unit = {
    val x = 17
    val y = 5
    val res = subY(0, x, y)
    println(res)
  }
}
