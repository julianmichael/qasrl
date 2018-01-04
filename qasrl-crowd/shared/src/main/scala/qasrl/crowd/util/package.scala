package qasrl.crowd

import cats.Foldable
import cats.implicits._

package object util {

  def dollarsToCents(d: Double): Int = math.round(100 * d).toInt

  object implicits extends PlatformSpecificImplicits {

    implicit class RichFoldable[F[_]: Foldable, A](val fa: F[A]) {

      def sum(implicit N: Numeric[A]): A = fa.foldLeft(N.fromInt(0))(N.plus)

      def meanOpt(implicit N: Numeric[A]): Option[Double] = {
        val (sum, count) = fa.foldLeft(N.fromInt(0), N.fromInt(0)) {
          case ((curSum, curCount), a) => (N.plus(curSum, a), N.plus(curCount, N.fromInt(1)))
        }
        if(count == 0) None else Some(N.toDouble(sum) / N.toDouble(count))
      }

      def proportion(predicate: A => Boolean): Double = fa.foldLeft((0, 0)) {
        case ((trues, total), a) =>
          if(predicate(a)) (trues + 1, total + 1)
          else (trues, total + 1)
      } match { case (trues, total) => trues.toDouble / total }

    }

    implicit class RichOption[A](val a: Option[A]) extends AnyVal {
      // more readable alternatives to forall/exists
      def emptyOr(predicate: A => Boolean): Boolean = a.forall(predicate)
      def nonEmptyAnd(predicate: A => Boolean): Boolean = a.exists(predicate)

      def ifEmpty[B](b: => B): Option[B] = a match {
        case Some(_) => None
        case None => Some(b)
      }
    }

  }

}
