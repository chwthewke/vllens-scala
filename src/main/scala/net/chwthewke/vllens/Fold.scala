package net.chwthewke.vllens

import cats._
import cats.data._
import cats.functor._
import cats.syntax.all._

import classes.First

abstract class Fold[S, A] {
  def runFold[F[_] : Contravariant : Applicative] : ( A => F[A] ) => ( S => F[S] )

  def foldMap[R : Monoid]( f : A => R ) : S => R = { s =>
    runFold[Const[R, ?]].apply( ( a : A ) => Const( f( a ) ) ).apply( s ).getConst
  }

  def preview( s : S ) : Option[A] = foldMap[First[A]]( ( a : A ) => First( a.some ) ).apply( s ).getFirst
}

object Fold {
  def folded[F[_] : Foldable, A] : Fold[F[A], A] =
    new Fold[F[A], A] {

      override def runFold[G[_] : Contravariant : Applicative] : ( A => G[A] ) => ( F[A] => G[F[A]] ) = runFold1[G]

      def runFold1[G[_] : Contravariant : Applicative] : ( A => G[A] ) => ( F[A] => G[F[A]] ) = {
        aga => fa => fa.traverse_[G, A]( aga ).map( _ => fa )
      }

      def runFold2[G[_] : Contravariant](implicit A: Applicative[G]) : ( A => G[A] ) => F[A] => G[F[A]] = {
        aga => fa =>
          fa.foldRight(Eval.always(A.pure(fa))) { (a, lgfa) =>
            lgfa.map(gfa => aga(a).map2(gfa)((_, b) => b))
          }

          ???
      }
    }
}
