package net.chwthewke.vllens

import cats._
import cats.data._

abstract class Lens[S, T, A, B] extends Traversal[S, T, A, B] {
  def runLens[F[_] : Functor] : ( A => F[B] ) => ( S => F[T] )

  override def runTraversal[F[_] : Applicative] = runLens[F]

  final def modify( f : A => B ) : S => T = over( f )

  final def get : S => A = runLens[Const[A, ?]].apply( Const.apply ).andThen( _.getConst )

  final def set( b : B ) : S => T = runLens[Id].apply( _ => b )
}

object Lens {
  def apply[S, T, A, B]( set : B => S => T, get : S => A ) : Lens[S, T, A, B] = new Lens[S, T, A, B] {
    override def runLens[F[_] : Functor] : ( A => F[B] ) => ( S => F[T] ) = {
      f => s => Functor[F].map( f( get( s ) ) )( set( _ )( s ) )
    }
  }
}
