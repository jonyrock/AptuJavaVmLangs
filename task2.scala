
case class Complex(re: Double, im: Double) {

  def this(str: String) = {
    this(Complex.parseRe(str), Complex.parseIm(str))
  }

  def this(d: Double) = {
    this(d, 0)
  }

  override def toString: String = {
    ("x" + re + " + " + im + "i")
      .replace("0.0 + 0.0i", "0.0")
      .replace(" + 0.0i", "")
      .replace("x0.0 + ", "")
      .replace("x", "")
  }

  def +(operand: Complex): Complex = new Complex(re + operand.re, im + operand.im)

  def -(operand: Complex): Complex = this + (-operand)

  def unary_- = new Complex(-re, -im)

  def unary_+ = new Complex(re, im)

  def *(operand: Complex) = new Complex(
    re * operand.re - im * operand.im,
    re * operand.im + im * operand.re
  )

  def /(operand: Complex) = new Complex(
    (re * operand.re + im * operand.im) /
      (operand.re * operand.re + operand.im * operand.im),
    (im * operand.re - re * operand.im) /
      (operand.re * operand.re + operand.im * operand.im)
  )

  def ^(y: Complex): Complex = {
    
    // x = a + jb
    // y = c + jd
    // x^y = ρ^c * e^(−d*θ) * [cos (c*θ + d * ln ρ) + j * sin (c * θ + d * ln ρ)]
    // ρ = abs(x)
    // θ = atan(b/a)
    val a = Re(this)
    val b = Im(this)
    val c = Re(y)
    val d = Im(y)
    val p = this.abs()
    
    val θ = if (a != 0) Math.atan(b / a) 
            else (Math.PI / 2) *  (b / Math.abs(b))
            
    
    val angle = c * θ + d * Math.log(p)
    // can't do it with implicit conversion
    new Complex(Math.pow(p, c) * Math.exp(-d * θ)) * Complex(Math.cos(angle), Math.sin(angle))

  }

  def abs() = math.sqrt(re * re + im * im)

  def sqrt() = {
    val r = abs()
    val sgn = if (im > 0) 1 else -1
    new Complex(math.sqrt((r + re) / 2), sgn * math.sqrt((r - re) / 2))
  }

  def conjugation = new Complex(re, -im)

}

object Complex {
  val I = new Complex(0, 1)

  private def parseIm(s: String): Double = {
    val clear = s.replace(" ", "").replace("i", "")
    val pi = clear.indexOf('+')
    if (pi == -1) {
      if (s.last == 'i') clear.substring(0).toDouble
      else 0.0
    }
    else clear.substring(pi + 1).toDouble
  }

  private def parseRe(s: String): Double = {
    val clear = s.replace(" ", "")
    val pi = clear.indexOf('+')
    if (pi == -1) {
      if (s.last == 'i') 0.0
      else clear.substring(0).toDouble
    }
    else clear.substring(0, pi).toDouble
  }

  implicit def Double2Complex(value: Double) = new Complex(value, 0.0)

}


object Re {
  def apply(c: Complex) = c.re
}

object Im {
  def apply(c: Complex) = c.im
}


