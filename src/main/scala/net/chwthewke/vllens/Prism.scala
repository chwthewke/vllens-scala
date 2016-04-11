package net.chwthewke.vllens

import cats._
import net.chwthewke.vllens.classes._

abstract class Prism[S, T, A, B] extends Traversal[S, T, A, B] {

  def runPrism[P[_, _] : Choice, F[_] : Applicative] : P[A, F[B]] => P[S, F[T]]

  override def runTraversal[F[_] : Applicative] : ( A => F[B] ) => ( S => F[T] ) =
    runPrism[Function1, F]

}

object Prism {
  implicit class SimplePrismOps[S, A]( val p : Prism[S, S, A, A] ) extends AnyVal {
    def review( a : A ) : S = p.runPrism[Tagged, Id].apply( Tagged( a ) ).untagged
  }
}
