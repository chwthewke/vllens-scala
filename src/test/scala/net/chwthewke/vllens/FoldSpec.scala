package net.chwthewke.vllens

import cats._
import cats.data._
import cats.std.list._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import scala.collection.{ mutable => scm }

class FoldSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  import FoldSpec._

  "getting a foldRight to stop evaluating" should {
    "work?" in {
      val rec = RecFoldable( List( 1, 2, 3 ) )

      def f( a : Int, b : Eval[Int] ) : Eval[Int] = if ( a >= 2 ) Eval.now( a ) else b

      val F = implicitly[Foldable[RecFoldable[List, ?]]]

      F.foldRight( rec, Eval.always( 0 ) )( f _ ).value should ===( 2 )

      rec.evals.toList should ===( List( 1, 2 ) )
    }

    "and with First as well?" in {
      pending
    }
  }

  "getting the first of a list" should {
    "evaluate the head only" in {

      val rec = RecFoldable( List( 1, 2, 3 ) )

      Fold.folded[RecFoldable[List, ?], Int].preview( rec ) should ===( Some( 1 ) )

      rec.evals.toList should ===( List( 1 ) )
    }
  }
}

object FoldSpec {

  case class RecFoldable[F[_], A]( self : F[A] ) {
    val evals : scm.Buffer[A] = new scm.ArrayBuffer[A]
  }

  implicit def recFoldable[F[_]]( implicit F : Foldable[F] ) : Foldable[RecFoldable[F, ?]] =
    new Foldable[RecFoldable[F, ?]] {
      override def foldLeft[A, B]( fa : RecFoldable[F, A], b : B )( f : ( B, A ) => B ) : B = {
        F.foldLeft( fa.self, b ) { ( b, a ) => fa.evals += a; f( b, a ) }
      }

      override def foldRight[A, B](
        fa : RecFoldable[F, A], lb : Eval[B] )(
          f : ( A, Eval[B] ) => Eval[B] ) : Eval[B] = {
        F.foldRight( fa.self, lb ) { ( a, lb ) => fa.evals += a; f( a, lb ) }
      }
    }
}
