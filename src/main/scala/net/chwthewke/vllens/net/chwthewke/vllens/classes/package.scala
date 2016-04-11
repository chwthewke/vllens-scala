package net.chwthewke.vllens.net.chwthewke.vllens

import algebra.Monoid

package object classes {

  case class Tagged[A, B]( untagged : B ) extends AnyVal

  case class First[A]( getFirst : A ) extends AnyVal

  implicit def FirstMonoid[A : Monoid] : Monoid[First[A]] =
    new Monoid[First[A]] {
      override def empty : First[A] = First( implicitly[Monoid[A]].empty )

      override def combine( x : First[A], y : First[A] ) : First[A] = x
    }
}
