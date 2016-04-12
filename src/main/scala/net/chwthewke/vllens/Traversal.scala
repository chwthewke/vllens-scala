package net.chwthewke.vllens

import cats._
import cats.data._

abstract class Traversal[S, T, A, B] {
  def runTraversal[F[_] : Applicative] : ( A => F[B] ) => ( S => F[T] )

  final def over( f : A => B ) : S => T = runTraversal[Id].apply( f )
}

object Traversal {
  def traverse[F[_] : Traverse, A, B] : Traversal[F[A], F[B], A, B] =
    new Traversal[F[A], F[B], A, B] {
      override def runTraversal[G[_] : Applicative] : ( A => G[B] ) => F[A] => G[F[B]] =
        f => t => Traverse[F].traverse[G, A, B]( t )( f )

    }
}
