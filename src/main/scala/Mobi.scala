
import breeze.math.Complex
import breeze.numerics.abs
import breeze.plot.{Figure, Plot}

import scala.math.pow

object Mobi {
  import MathHelpers._
  import NumericalIntegration._

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

    val α_L_arr = Seq(0.001 / L, 0.01 / L, 0.1 / L, 1.0 / L)
    val pout_przez_ps_arr = Seq(0.001, 0.01, 0.1, 1.0)

    val input = Seq(
      (   0.01,   .744380E+03, .419876E+03),
      (   0.03,   .620847E+03, .412581E+03),
      (   0.05,   .562817E+03, .408455E+03),
      (   0.07,   .524365E+03, .405420E+03),
      (   0.1,    .483398E+03, .401880E+03),
      (   0.3,    .355847E+03, .388266E+03),
      (   0.5,    .295886E+03, .380053E+03),
      (   0.7,    .256323E+03, .373708E+03),
      (   1.0,    .214502E+03, .365983E+03),
      (   3.0,    .981079E+02, .335818E+03),
      (   5.0,    .613037E+02, .324438E+03),
      (   7.0,    .443115E+02, .319909E+03),
      (  10.0,    .312256E+02, .317127E+03),
      (  30.0,    .104614E+02, .314497E+03),
      (  50.0,    .628052E+01, .314275E+03),
      (  70.0,    .448608E+01, .314214E+03),
      ( 100.0,    .314178E+01, .314181E+03),
      ( 300.0,    .104713E+01, .314153E+03),
      ( 500.0,    .628281E+00, .314151E+03),
      ( 700.0,    .448799E+00, .314151E+03),
      (1000.0,    .314140E+00, .314150E+03)
    ).map { case (kl_, x, y) =>
      kl_ -> Complex(x, y)
    }

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


    val fig1 = Figure()
    val p1 = fig1.subplot(0)

    val skok_dx = 225300
    val skok_dy = 225300

    val d = 1 to 6 map { m=>
      println(s"Obliczam d dla m=$m")

      val max_κx = domainEnd(d_x(m))
      val max_κy = domainEnd(d_y(m))
      val min_k_dx = findMin(d_x(m), 0.0, max_κx)
      val min_k_dy = findMin(d_y(m), 0.0, max_κy)
      val min_dx = d_x(m)(min_k_dx)
      val min_dy = d_y(m)(min_k_dy)

      println(s"max_κx = $max_κx")
      println(s"max_κy = $max_κy")
      println(s"min_k_dx = $min_k_dx")
      println(s"min_k_dy = $min_k_dy")
      println(s"min_dx = $min_dx")
      println(s"min_dy = $min_dy")

      do_plot(p1, d_x(m), 0, max_κx max max_κy)
      do_plot(p1, d_y(m), 0, max_κx max max_κy)

      min_dx -> min_dy
    }

    val κd = 1 to 5 map { m =>
      println(s"Obliczam κ i β dla m=$m")

      val (nastepny_poziom_dx, nastepny_poziom_dy) = d(m)
      val nastepny_poziom = nastepny_poziom_dx min nastepny_poziom_dy

      val min_κx = findMin((κ: Double) => Math.abs(d_x(m)(κ) - nastepny_poziom), 1e-5, skok_dx)
      val min_κy = findMin((κ: Double) => Math.abs(d_y(m)(κ) - nastepny_poziom), 1e-5, skok_dy)

      println(s"min_κx = $min_κx")
      println(s"min_κy = $min_κy")

      min_κx -> min_κy
    }

    fig1.saveas("plot1.png")

    println
    println

    val fig2 = Figure()
    val p2 = fig2.subplot(0)

    val skok_tx = 3340000
    val skok_ty = 4340000

    val t = 1 to 6 map { n=>
      println(s"Obliczam t dla n=$n")

      val max_κx = domainEnd(t_x(n), 10)
      val max_κy = domainEnd(t_y(n), 10)
      val min_k_tx = findMin(t_x(n), 0.0, max_κx)
      val min_k_ty = findMin(t_y(n), 0.0, max_κy)
      val min_tx = t_x(n)(min_k_tx)
      val min_ty = t_y(n)(min_k_ty)

      println(s"max_κx = $max_κx")
      println(s"max_κy = $max_κy")
      println(s"min_k_tx = $min_k_tx")
      println(s"min_k_ty = $min_k_ty")
      println(s"min_tx = $min_tx")
      println(s"min_ty = $min_ty")

      do_plot(p2, t_x(n), 0, max_κx max max_κy)
      do_plot(p2, t_y(n), 0, max_κx max max_κy)

      min_tx -> min_ty
    }

    fig2.saveas("plot2.png")

    val κt = 1 to 5 map { n =>
      println(s"Obliczam κ i β dla n=$n")

      val (nastepny_poziom_tx, nastepny_poziom_ty) = t(n)
      val nastepny_poziom = nastepny_poziom_tx min nastepny_poziom_ty

      val min_κx = findMin((κ: Double) => Math.abs(t_x(n)(κ) - nastepny_poziom), 1e-5, skok_tx)
      val min_κy = findMin((κ: Double) => Math.abs(t_y(n)(κ) - nastepny_poziom), 1e-5, skok_ty)

      println(s"min_κx = $min_κx")
      println(s"min_κy = $min_κy")

      min_κx -> min_κy
    }

    // bety
    val βl = 0 to 4 map { n =>
      val βx = Math.sqrt((n_1 ^ 2) * (k_0 ^ 2) - (κd(n)._1 ^ 2) - (κt(n)._1 ^ 2))
      val βy = Math.sqrt((n_1 ^ 2) * (k_0 ^ 2) - (κd(n)._2 ^ 2) - (κt(n)._2 ^ 2))

      println(s"n=${n+1}")
      println(s"βx=$βx")
      println(s"βy=$βy")
      βx -> βy
    }

    def κ_xx(m: Int, n: Int) = {
      κd(n)._1
    }
    def κ_xy(m: Int, n: Int) = {
      κt(n)._1
    }

    def βx(m: Int, n: Int) = {
      βl(m-1)._1
    }

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

    val ex11T: ((Double, Double)) => Double = (Ex(1, 1)).tupled
    val Ex11: Cache[(Double, Double), Double] = Cache((x: (Double, Double)) => ex11T(x))

//    val marginesy = Seq(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 2, 5, 10)

    val margines = 2.0

    val lewa_calka = NumericalIntegration.integrate2({ (x, y) => abs(Ex11((x, y))) ^ 2 }, -margines * d(1)._2, (1+margines) * d(1)._2, -margines * t(1)._2, (1+margines) * t(1)._2)

    α_L_arr.foreach { α_L =>
      val fig_g = Figure()
      val p_g = fig_g.subplot(0)
      p_g.title = s"α_L = $α_L"

      println(s"α_L = ${α_L}")
      pout_przez_ps_arr.foreach { pout_przez_ps =>
        println(s"pout_przez_ps = $pout_przez_ps")

        println("klq\tγq\tg0")
        val res: Seq[(Double, Double)] = input.map { case (klq: Double, γq: Complex) =>

          val C = 2 * (abs(sinh(γq * L))^2)

          def frq(z: Double) = sinh(γq * (z + L / 2))
          def fsq(z: Double) = sinh(γq * (z - L / 2))

          val suma_kw_f = Cache((z: Double) => (abs(frq(z)) ^ 2) + (abs(fsq(z)) ^ 2))

          val licznik_g0 = C + 2 * α_L * integrate((z: Double) => (abs(frq(z)) ^ 2) + (abs(fsq(z)) ^ 2), -L/2, L/2)
          val lewy_ulamek_mianownika = 2.0 / lewa_calka

          def fkcja_pod_potrojna_calka(x: Double, y: Double, z: Double) = {
            val E_x = Ex11((x, y))

            val licz = suma_kw_f(z) * (abs(E_x) ^ 2)
            val mian = 1 + pout_przez_ps*((suma_kw_f(z) * (abs(E_x)^2)) / C)

            licz / mian
          }

          val potr_calka = integrate3(fkcja_pod_potrojna_calka, -L/2, L/2, -margines*d(1)._2, (1+margines)*d(1)._2, -margines*t(1)._2, (1+margines)*t(1)._2)

          val g0 = licznik_g0 / (lewy_ulamek_mianownika * potr_calka)

          println(s"$klq\t$γq\t$g0")

          klq -> g0
        }

        p_g += breeze.plot.plot(
          res.map(_._1) map Math.log10,
          res.map(_._2) map Math.log10)
        println
        println
      }
      fig_g.saveas("plot3.png")
    }
  }


  def do_plot(p: Plot, f: Double=>Double, a: Double, b: Double, title: String = "Wykres") = {
    import breeze.plot._

    val x = a to b by ((b - a) / 10000) drop 1
    val y = x.map(f) map (Math.log10)
    p += plot(x, y, '-')
    p.title = title
    p.xlabel = "x axis"
    p.ylabel = "y axis"

  }

  override def toString = s"Mobi()"
}

object NumericalIntegration {
  def leftRect(f:Double=>Double, a:Double, b:Double)=f(a)
  def midRect(f:Double=>Double, a:Double, b:Double)=f((a+b)/2)
  def rightRect(f:Double=>Double, a:Double, b:Double)=f(b)
  def trapezoid(f:Double=>Double, a:Double, b:Double)=(f(a)+f(b))/2
  def simpson(f:Double=>Double, a:Double, b:Double)=(f(a)+4*f((a+b)/2)+f(b))/6

  type Method = (Double=>Double, Double, Double) => Double
  def integrate(f:Double=>Double, a:Double, b:Double, steps:Int = 20000, m: Method = midRect _)={
    val delta:Double=(b-a)/steps
    val xs = a until b by delta

    val ranges = 1
    val xs_split = xs.grouped(steps/ranges).toSeq

    val subsums = xs_split.seq.map { xss =>
      xss.foldLeft(0.0)((s, x) => s+m(f, x, x+delta))
    }

    delta * subsums.foldLeft(0.0)((s, x) => s + x)
  }

  // przy 400 jest	1.1766891643319607E27
  // przy 600 jest  2.2285952639501582E24
  // przy 800 jest  1.02249518994718E23

  def integrate2(f:(Double, Double)=>Double, a1:Double, b1:Double, a2:Double, b2:Double,
                 steps: Int = 3000, m: Method = midRect _) = {
    def inner(v: Double) = integrate(f.curried(v), a2, b2, steps, m)
    integrate(inner, a1, b1, steps, m)
  }

  def integrate3(f:(Double, Double, Double)=>Double, a1:Double, b1:Double, a2:Double, b2:Double, a3: Double, b3: Double,
                 steps: Int = 1000, m: Method = midRect _) = {
    def inner(v: Double) = integrate2({case(vv, vvv)=>f(v,vv,vvv)}, a2, b2, a3, b3, steps/3, m)
    integrate(inner, a1, b1, steps, m)
  }
}

object MathHelpers {
  def exp(z: Complex) = Complex(Math.cos(z.imag), Math.sin(z.imag)) * Math.exp(z.real)
  def sinh(z: Complex): Complex = (exp(z) - exp(-z)) / 2
  def exp(x: Double) = Math exp x

  def findMin(f: (Double => Double), a: Double, b: Double, baseSteps: Int = 50000, accuracy: Double = 1e-10): Double = {
    val diff = (b - a) / baseSteps
    val steps = a to b by diff
    val min = steps.filterNot(e => f(e).isNaN).minBy(f)
    if (diff < accuracy)
      min
    else
      findMin(f, (min-diff) max a, (min+diff) min b, baseSteps, accuracy)
  }

  def domainEnd(f: (Double => Double), initialStep: Int = 1) = {
    val firstNan = Stream.from(0, step = initialStep).map(_.toDouble).find(e=>f(e).isNaN).get
    firstNan - initialStep
  }

  case class Cache[K, V](f: K => V){
    val cache: scala.collection.mutable.Map[K, V] = scala.collection.mutable.Map.empty[K, V]

    def get(k: K): V =
      cache.getOrElse(k, {
        val newValue = f(k)
        cache += (k -> newValue)
        newValue
      })

    def apply(k: K): V = get(k)
  }
}
