package qasrl.util

sealed trait DependentPair[F[_], G[_]] {
  type A
  val fst: F[A]
  val snd: G[A]
}
object DependentPair {
  private[this] case class DependentPairImpl[F[_], G[_], A0](
    override val fst: F[A0],
    override val snd: G[A0]
  ) extends DependentPair[F, G] { type A = A0 }

  def apply[F[_], G[_], A0](fst: F[A0], snd: G[A0]): DependentPair[F, G] { type A = A0 } =
    DependentPairImpl[F, G, A0](fst, snd)
}
