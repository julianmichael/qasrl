package qasrl.util

import cats.Order
import cats.data.NonEmptyList
import cats.data.Ior

object implicits {
  implicit class RichOption[A](val a: Option[A]) extends AnyVal {
    def ifEmpty[B](b: => B): Option[B] = a match {
      case Some(_) => None
      case None => Some(b)
    }
  }

  implicit class RichNonEmptyList[A](val as: NonEmptyList[A]) extends AnyVal {
    def partition[B, C](f: A => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]] = {
      val init = f(as.head).fold(
        b => Ior.Left(NonEmptyList.of(b)),
        c => Ior.Right(NonEmptyList.of(c))
      ): Ior[NonEmptyList[B], NonEmptyList[C]]
      as.tail.map(f).foldLeft(init) {
        case (Ior.Left(bs), Left(b)) => Ior.Left(b :: bs)
        case (Ior.Right(cs), Left(b)) => Ior.Both(NonEmptyList.of(b), cs)
        case (Ior.Both(bs, cs), Left(b)) => Ior.Both(b :: bs, cs)
        case (Ior.Left(bs), Right(c)) => Ior.Both(bs, NonEmptyList.of(c))
        case (Ior.Right(cs), Right(c)) => Ior.Right(c :: cs)
        case (Ior.Both(bs, cs), Right(c)) => Ior.Both(bs, c :: cs)
      }
    }

    // taken from latest cats; holdover until version upgrade
    def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyList[AA] = {
      NonEmptyList.fromListUnsafe(as.toList.sorted(AA.toOrdering))
    }
  }
}
