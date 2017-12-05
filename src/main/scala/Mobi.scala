
import scala.math.pow

object Mobi {

  implicit class PowerDouble(val i: Double) extends AnyVal {
    def ^ (exp: Int):Double = pow(i, exp)
  }

  trait Mod

  case class Ex(m: Int, n: Int) extends Mod
  case class Ey(m: Int, n: Int) extends Mod

  def main(args: Array[String]): Unit = {

    val min_κ = 0
    val max_κ = 225650

    val π = Math.PI

    val n_1 = 1.53
    val n_2 = 1.529
    val (n_3, n_4, n_5) = (1.0, 1.0, 1.0)

    val λ = 1.54e-6 // m
    val L = 0.05    // m

    val k_0 = 2 * π / λ

    def γ_2x(κ_xx: Double) = Math.sqrt((k_0^2) * ((n_1^2) - (n_2^2)) - (κ_xx^2))
    def γ_3x(κ_xx: Double) = Math.sqrt((k_0^2) * ((n_1^2) - (n_3^2)) - (κ_xx^2))
    def γ_4x(κ_xx: Double) = Math.sqrt((k_0^2) * ((n_1^2) - (n_4^2)) - (κ_xx^2))
    def γ_5x(κ_xx: Double) = Math.sqrt((k_0^2) * ((n_1^2) - (n_5^2)) - (κ_xx^2))

    def γ_2y(κ_yy: Double) = Math.sqrt((k_0^2) * ((n_1^2) - (n_2^2)) - (κ_yy^2))
    def γ_3y(κ_yy: Double) = Math.sqrt((k_0^2) * ((n_1^2) - (n_3^2)) - (κ_yy^2))
    def γ_4y(κ_yy: Double) = Math.sqrt((k_0^2) * ((n_1^2) - (n_4^2)) - (κ_yy^2))
    def γ_5y(κ_yy: Double) = Math.sqrt((k_0^2) * ((n_1^2) - (n_5^2)) - (κ_yy^2))

    def d_x(m_x: Int)(κ_xx: Double) = {
      def licznik_arctg =
        (n_1 ^ 2) * κ_xx * ((n_3 ^ 2) * γ_2x(κ_xx) + (n_2 ^ 2) * γ_3x(κ_xx))

      def mianownik_arctg =
        (n_2 ^ 2) * (n_3 ^ 2) * (κ_xx ^ 2) - (n_1 ^ 4) * γ_2x(κ_xx) * γ_3x(κ_xx)

      (Math.atan(licznik_arctg / mianownik_arctg) + m_x * π) / κ_xx
    }

    def d_y(m_y: Int)(κ_yx: Double) = {
      def licznik_arctg =
        κ_yx * (γ_2y(κ_yx) + γ_3y(κ_yx))

      def mianownik_arctg =
        (κ_yx ^ 2) - γ_2y(κ_yx) * γ_3y(κ_yx)

      (Math.atan(licznik_arctg / mianownik_arctg) + m_y * π) / κ_yx
    }

    def t_x(n_x: Int)(κ_xy: Double) = {
      def licznik_arctg =
        κ_xy * (γ_4x(κ_xy) + γ_5x(κ_xy))

      def mianownik_arctg =
        (κ_xy ^ 2) - γ_4x(κ_xy) * γ_5x(κ_xy)

      (Math.atan(licznik_arctg / mianownik_arctg) + n_x * π) / κ_xy
    }

    def t_y(n_y: Int)(κ_yy: Double) = {
      def licznik_arctg =
        (n_1 ^ 2) * κ_yy * ((n_4 ^ 2) * γ_4y(κ_yy) + (n_5 ^ 2) * γ_5y(κ_yy))

      def mianownik_arctg =
        (n_4 ^ 2) * (n_5 ^ 2) * (κ_yy ^ 2) - (n_1 ^ 4) * γ_4y(κ_yy) * γ_5y(κ_yy)

      (Math.atan(licznik_arctg / mianownik_arctg) + n_y * π) / κ_yy
    }

    val d = 1 to 5 map { m =>

      println(s"m = $m")

//      val start = 225600.0
//      val end = 225700.0

      val start = 0.0
      val end = 5000000.0

      val numSteps = 10000
      val steps = (0 to numSteps) map (e => e * (end - start) / numSteps + start)

      val funValuesX = steps map (step => step -> d_x(m+1)(step))// map {s => println(s"${s._1} ${s._2} "); s}
      val funValuesY = steps map (step => step -> d_y(m+1)(step))// map {s => println(s"${s._1} ${s._2} "); s}

      println

      val minX = funValuesX.minBy(_._2)
      val minY = funValuesY.minBy(_._2)
      println(s"graniczna κ: ${minX._1}\t\td_x = ${minX._2}")
      println(s"graniczna κ: ${minY._1}\t\td_y = ${minY._2}")

      minX._2 -> minY._2
    }

    val t = 1 to 6 map { n =>

      println(s"n = $n")

//      val start = 4000000.0
//      val end = 5000000.0
      val start = 0.0
      val end = 5000000.0

      val numSteps = 10000
      val steps = (0 to numSteps) map (e => e * (end - start) / numSteps + start)

      val funValuesX = steps map (step => step -> t_x(n+1)(step))// map {s => println(s"${s._1} ${s._2} "); s}
      val funValuesY = steps map (step => step -> t_y(n+1)(step))// map {s => println(s"${s._1} ${s._2} "); s}

      println

      val minX = funValuesX.minBy(_._2)
      val minY = funValuesY.minBy(_._2)
      println(s"graniczna κ: ${minX._1}\t\tt_x = ${minX._2}")
      println(s"graniczna κ: ${minY._1}\t\tt_y = ${minY._2}")

      minX._2 -> minY._2
    }

    def κ_xx(m: Int, n: Int) = {
      val dx = 20.89e-6//d(m-1)._1 // 1-based

      //        val start = 0.0
      //        val end = 250000.0

      val start = 0.0
      val end = 5000000.0

      val numSteps = 1000000
      val steps = (0 to numSteps) map (e => e * (end - start) / numSteps + start)

      val funValuesX = steps map (step => step -> Math.abs(d_x(1)(step) - dx))
      val minX = funValuesX.minBy(_._2)

      println(s"κ_xx dla m=$m: ${minX._1}")

      minX._1
    }

    def κ_xy(m: Int, n: Int) = {
      val tx = 1.08e-6//t(n-1)._1 // 1-based

      //        val start = 0.0
      //        val end = 5000000.0

      val start = 0.0
      val end = 5000000.0

      val numSteps = 1000000
      val steps = (0 to numSteps) map (e => e * (end - start) / numSteps + start)

      val funValuesY = steps map (step => step -> Math.abs(t_x(1)(step) - tx))
      val minY = funValuesY.minBy(_._2)

      println(s"κ_xy dla n=$n: ${minY._1}")

      minY._1
    }

    def βx(m: Int, n: Int) = {

      val β = Math.sqrt((n_1^2)*(k_0^2) - (κ_xx(m,n) ^ 2) - (κ_xy(m,n) ^ 2))
      println(s"βx($m, $n) = $β")
      β
    }

    def κ_yx(m: Int, n: Int) = {
      val dy = d(m-1)._2 // 1-based
//      val dy = 20.89e-6//d(m-1)._2 // 1-based

      val start = 0.0
      val end = 5000000.0

      val numSteps = 1000000
      val steps = (0 to numSteps) map (e => e * (end - start) / numSteps + start)

      val funValuesX = steps map (step => step -> Math.abs(d_y(1)(step) - dy))
      val minX = funValuesX.minBy(_._2)

      println(s"κ_yx dla m=$m: ${minX._1}")

      minX._1
    }

    def κ_yy(m: Int, n: Int) = {
      val ty = t(n-1)._2 // 1-based
//      val ty = 1.08e-6//t(n-1)._2 // 1-based

      val start = 0.0
      val end = 5000000.0

      val numSteps = 1000000
      val steps = (0 to numSteps) map (e => e * (end - start) / numSteps + start)

      val funValuesY = steps map (step => step -> Math.abs(t_y(1)(step) - ty))
      val minY = funValuesY.minBy(_._2)

      println(s"κ_yy dla n=$n: ${minY._1}")

      minY._1
    }

    def βy(m: Int, n: Int) = {

      val β = Math.sqrt((n_1^2)*(k_0^2) - (κ_yx(m,n) ^ 2) - (κ_yy(m,n) ^ 2))
      println(s"βy($m, $n) = $β")
      β
    }

    βx(1,1)
    βx(1,2)
    βx(1,3)
    βx(1,4)
    βx(1,5)
    βx(2,1)
    βx(3,1)
    βx(4,1)
    βx(5,1)

    βy(1,1)
    βy(1,2)
    βy(1,3)
    βy(1,4)
    βy(1,5)
    βy(2,1)
    βy(3,1)
    βy(4,1)
    βy(5,1)

    def ξ_x(m: Int, n: Int) =
      (Math.atan(((-n_3^2) * κ_xx(m,n)) / ((n_1^2) * γ_3x(κ_xx(m,n)))) + m*π ) / κ_xx(m,n)

    def η_x(m: Int, n: Int) =
      (Math.atan(-γ_5x(κ_xy(m,n)) / κ_xy(m,n)) + n*π ) / κ_xy(m,n)


    def Ex(m: Int, n: Int) = {

      val d_graniczne = d_x(m_x = m)(κ_xx(m, n))
      val t_graniczne = t_x(n_x = n)(κ_xy(m, n))

      val i = 1

      val κx = κ_xx(m, n)
      val κy = κ_xy(m, n)

      val β = βx(m, n)
      val η = η_x(m, n)
      val ξ = ξ_x(m, n)

      val γ2 = γ_2x(κx)
      val γ3 = γ_3x(κx)
      val γ4 = γ_4x(κx)
      val γ5 = γ_5x(κx)

      val Emn = (x: Double, y: Double) => (x, y) match {
        case (x, y) if (x >= -d_graniczne && x <= 0) && (y >=0 && y <= t_graniczne) =>
          // w warstwie falowodowej
          (i / (κx * β)) * ((n_1^2) * (k_0^2) - (κx^2)) * Math.cos(κy * (y + η)) * Math.sin(κx * (x + ξ))

        case (x, y) if (x <= (0-d_graniczne)) && (y >=0 && y <= t_graniczne) =>
          // w podłożu
          (i / (γ2 * β)) * ((γ2^2) + (n_2^2) * (k_0^2)) * Math.cos(κy * (y + η)) * Math.cos(κx * (ξ - d_graniczne)) * Math.exp(γ2 * (x + d_graniczne))

        case (x, y) if (x >= 0) && (y >=0 && y <= t_graniczne) =>
          // w pokryciu
          (-i / (γ_3x(κx) * β)) * ((γ_3x(κx)^2) + (n_2^2)*(k_0^2)) * Math.cos(κy * (y + η)) * Math.cos(κx * ξ) * Math.exp(-γ3 * x)

        case (x, y) if (x <= 0 && x >= -d_graniczne) && y >= t_graniczne =>
          // w warstwie z lewej strony
          (i * (n_1^2) / (n_4^2)) * (((n_4^2)*(k_0^2) - (κx^2)) / (κx * β)) * Math.cos(κy * (t_graniczne + η)) * Math.sin(κx * (x + ξ)) * Math.exp(-γ4 * (y - t_graniczne))

        case (x, y) if (x >= -d_graniczne && x < 0) && (y <= 0) =>
          // w warstwie z prawej strony
          (i*(n_1^2)/(n_5^2)) * (((n_5^2) * (k_0^2) - (κx^2))/(κx * β)) * Math.cos(κy*η) * Math.sin(κx*(x + ξ)) * Math.exp(γ5 * y)

        case _ => 0
      }

      println(s"m=$m n=$n")

      Emn
    }

    val lewa_calka = NumericalIntegration.integrate2(Ex(1,1), -1*d(1)._2, 1*d(1)._2, -1*t(1)._2, 1*t(1)._2)
    println("wartosc calki:")
    println(lewa_calka)

  }
}

object NumericalIntegration {
  def leftRect(f:Double=>Double, a:Double, b:Double)=f(a)
  def midRect(f:Double=>Double, a:Double, b:Double)=f((a+b)/2)
  def rightRect(f:Double=>Double, a:Double, b:Double)=f(b)
  def trapezoid(f:Double=>Double, a:Double, b:Double)=(f(a)+f(b))/2
  def simpson(f:Double=>Double, a:Double, b:Double)=(f(a)+4*f((a+b)/2)+f(b))/6

  type Method = (Double=>Double, Double, Double) => Double
  def integrate(f:Double=>Double, a:Double, b:Double, steps:Int = 2000, m: Method = simpson _)={
    val delta:Double=(b-a)/steps
    delta*(a to b by delta).foldLeft(0.0)((s,x) => s+m(f, x, x+delta))
  }

  def integrate2(f:(Double, Double)=>Double, a1:Double, b1:Double, a2:Double, b2:Double,
                 steps: Int = 1000, m: Method = midRect _) = {
    def inner(v: Double) = integrate(f.curried(v), a2, b2, steps, m)
    integrate(inner, a1, b1, steps, m)
  }
}