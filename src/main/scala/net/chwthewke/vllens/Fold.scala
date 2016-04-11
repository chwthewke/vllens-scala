package net.chwthewke.vllens

import cats._
import cats.functor._
import cats.syntax.all._

abstract class Fold[S, A] {
  def apply[F[_] : Contravariant : Applicative] : ( A => F[A] ) => ( S => F[S] )

  def preview( s : S ) : Option[A] = ???
}

object Fold {
  def folded[F[_] : Foldable, A] : Fold[F[A], A] =
    new Fold[F[A], A] {
      override def apply[G[_] : Contravariant : Applicative] : ( A => G[A] ) => ( F[A] => G[F[A]] ) = {
        aga => fa => fa.traverse_[G, A]( aga ).map( _ => fa )
      }
    }
}
