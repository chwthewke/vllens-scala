package net.chwthewke.vllens

import cats._

package object classes {

  case class Tagged[A, B]( untagged : B ) extends AnyVal

  case class First[A]( getFirst : Option[A] ) extends AnyVal

  // TODO Lazy?
  implicit def FirstMonoid[A] : Monoid[First[A]] =
    new Monoid[First[A]] {
      override def empty : First[A] = First( None )

      override def combine( x : First[A], y : First[A] ) : First[A] =
        x.getFirst.fold( y )( _ => x )
    }
}
