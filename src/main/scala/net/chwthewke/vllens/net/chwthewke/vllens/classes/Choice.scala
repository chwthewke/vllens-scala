package net.chwthewke.vllens.net.chwthewke.vllens.classes

import cats.data._
import cats.functor._
import cats.syntax.xor._

trait Choice[F[_, _]] extends Profunctor[F] {

  def left[A, B, C]( fab : F[A, B] ) : F[A Xor C, B Xor C] =
    dimap[C Xor A, C Xor B, A Xor C, B Xor C]( right( fab ) )( _.swap )( _.swap )

  def right[A, B, C]( fab : F[A, B] ) : F[C Xor A, C Xor B] =
    dimap[A Xor C, B Xor C, C Xor A, C Xor B]( left( fab ) )( _.swap )( _.swap )

}

object Choice {
  implicit val Function1Choice : Choice[Function1] =
    new Choice[Function1] {
      override def dimap[A, B, C, D]( fab : A => B )( f : C => A )( g : B => D ) : C => D =
        implicitly[Profunctor[Function1]].dimap( fab )( f )( g )

      override def right[A, B, C]( fab : A => B ) : Xor[C, A] => Xor[C, B] = {
        ca => ca.map( fab )
      }
    }

  implicit val TaggedChoice : Choice[Tagged] =
    new Choice[Tagged] {
      override def dimap[A, B, C, D]( fab : Tagged[A, B] )( f : C => A )( g : B => D ) : Tagged[C, D] =
        Tagged( g( fab.untagged ) )

      override def right[A, B, C]( fab : Tagged[A, B] ) : Tagged[Xor[C, A], Xor[C, B]] =
        Tagged( fab.untagged.right )
    }
}
