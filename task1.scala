
object HelloScala extends App {

  // Task 1
  def extendedGcd(a: Long, b: Long): (Long, Long, Long) = {
    def extendedAbsoluteGcd(a: Long, b: Long): (Long, Long, Long) = {
      if (b == 0)
        return (a, 1, 0)
      val (d_, x_, y_) = extendedAbsoluteGcd(b, a % b)
      (d_, y_, x_ - (a / b) * y_)
    }
    val (d, x, y) = extendedAbsoluteGcd(Math.abs(a), Math.abs(b))
    (d, x * (a / Math.abs(a)), y * (b / Math.abs(b)))
  }

  def gcd(a: Long, b: Long) = extendedGcd(a, b)._1

  // Task 2
  class CoStack[+T](private val data: List[T] = Nil) {

    def push[B >: T](x: B): CoStack[B] = {
      new CoStack(x :: data)
    }

    def pop(): (T, CoStack[T]) = {
      (data.head, new CoStack(data.tail))
    }

    override def toString() = data.foldLeft("")((a, b) => a + " " + b)

  }

  // Task 3
  def quickSort[T: Ordering](xs: Array[T]): Unit = {

    def swap(p: Int, q: Int): Unit = {
      if (p == q) return
      val t = xs(p)
      xs.update(p, xs(q))
      xs.update(q, t)
    }

    def partition(p: Int, r: Int): Int = {
      import scala.math.Ordering.Implicits._
      val x = xs(r - 1)
      var i = p - 1
      var j = p
      while (j <= r - 2) {
        if (xs(j) <= x) {
          i += 1
          swap(i, j)
        }
        j += 1
      }
      swap(i + 1, r - 1)
      i + 1
    }

    def qSort(p: Int, r: Int): Unit = {
      if (p >= r - 1)
        return
      val q = partition(p, r)
      qSort(p, q)
      qSort(q + 1, r)
    }

    qSort(0, xs.length)

  }

  // Task 4
  class BInt(private val data: List[Int] = List(0), val isNegative: Boolean = false) extends Ordered[BInt] {

    if (data.equals(Nil))
      throw new IllegalArgumentException("Nil is bad. Use List(0)")

    def this(v: Long) = {
      this(Math.abs(v).toString.toCharArray.map(x => x - '0').toList.reverse, v < 0)
    }

    def +(b: BInt): BInt = {
      if (isNegative == b.isNegative)
        return new BInt(absPlus(b), isNegative)
      val comp = absCompare(b.data)
      if (comp == 0) return new BInt()
      if (comp == 1) return new BInt(absMinus(b.data), isNegative)
      new BInt(b.absMinus(data), b.isNegative)
    }

    def -(b: BInt): BInt = this + new BInt(b.data, !b.isNegative)

    def *(b: BInt): BInt = {
      if (isNegative == b.isNegative)
        return new BInt(absMult(b))
      new BInt(absMult(b), true)
    }

    def /(b: BInt): BInt = {
      if (isNegative == b.isNegative)
        return new BInt(absDiv(b))
      new BInt(absDiv(b), true)
    }

    def <<(shift: Int): BInt = new BInt(List.fill(shift)(0) ++ data, isNegative)

    override def compare(b: BInt): Int = {
      if (!isNegative && !b.isNegative) return absCompare(b.data)
      if (isNegative && b.isNegative) return -absCompare(b.data)
      if (!isNegative && b.isNegative) 1 else -1
    }

    override def equals(other: Any) = other match {
      case b: BInt => compare(b) == 0
      case _ => false
    }

    override def toString = data.reverse.foldLeft(if (isNegative) "-" else "")((a, b) => a + b)

    private def absPlus(b: BInt): List[Int] = {
      val as = b.data
      val bs = data
      var prev = 0
      val listPlus = (for (i <- 0 to (Math.max(as.length, bs.length) - 1)) yield {
        val a = if (as.length > i) as(i) else 0
        val b = if (bs.length > i) bs(i) else 0
        val res = a + b + prev
        prev = res / 10
        res % 10
      }).toList.reverse
      val res = if (prev == 0) listPlus else prev :: listPlus
      res.reverse
    }

    private def absMinus(bs: List[Int]): List[Int] = {
      // as always strongly bigger
      val as = data
      var prev = 0
      val listDiff = (for (i <- 0 to as.length - 1) yield {
        val a = as(i)
        val b = if (bs.length > i) bs(i) else 0
        val res = a - b - prev
        prev = if (res < 0) 1 else 0
        (res + 10) % 10
      }).toList
      listDiff.reverse.dropWhile(_ == 0).reverse
    }

    private def absCompare(bss: List[Int]): Int = {
      val as = data.reverse
      val bs = bss.reverse
      if (as.length < bs.length)
        return -1
      if (as.length > bs.length)
        return 1
      for ((p, q) <- as.zip(bs)) {
        if (p < q)
          return -1
        if (p > q)
          return 1
      }
      0
    }

    private def absMult(b: BInt): List[Int] = {
      val as = data
      val bs = b.data
      val cnSum = Array.fill(as.length + bs.length - 1)(new BInt())
      as.zipWithIndex.foreach {
        case (a, i) =>
          bs.zipWithIndex.foreach {
            case (b, j) =>
              cnSum.update(i + j, cnSum(i + j) + new BInt(a * b))
          }
      }

      cnSum.zipWithIndex.map {
        case (b, i) => b << i
      }.foldLeft(new BInt())(_ + _).data
    }

    private def absDiv(b: BInt): List[Int] = {

      def findBestPower(p: BInt, q: BInt): BInt = {
        if (p < q)
          return new BInt()

        def biSearch(left: Int = 0, right: Int = p.data.length): Int = {
          if (left >= right)
            return left
          val middleI = (left + right) / 2
          val middleVal = (new BInt(1) << middleI) * q
          if (p == middleVal)
            return middleI
          if (p < middleVal)
            return biSearch(left, middleI - 1)
          biSearch(middleI + 1, right)
        }

        var res = biSearch()
        if (p < (new BInt(1) << res) * q)
          res = res - 1

        for (i <- 2 to 9) {
          if (p < (new BInt(i) << res) * q)
            return new BInt(i - 1) << res
        }

        new BInt(9) << res

      }


      val curDigit = findBestPower(new BInt(this.data), new BInt(b.data))
      var res = curDigit
      var currentDividend = new BInt(this.data) - (curDigit * b)

      for (i <- 1 to curDigit.data.length) {
        val curRes = findBestPower(currentDividend, b)
        val curMul = curRes * b
        currentDividend = currentDividend - curMul
        res = res + curRes
      }

      res.data

    }

  }

}