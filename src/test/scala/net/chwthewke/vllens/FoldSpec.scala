package net.chwthewke.vllens

import cats._
import org.scalatest.{ Matchers, WordSpec }
import scala.collection.{ mutable => scm }

class FoldSpec extends WordSpec with Matchers {

  import FoldSpec._

  "getting the first of a list" should {
    "evaluate the head only" in {

      val rec = RecFoldable( List( 1, 2, 3 ) )

      Fold.folded[RecFoldable, Int].preview( rec ) should ===( Some( 1 ) )

      rec.evals.toList should ===( List( 1 ) )
    }
  }
}

object FoldSpec {

  case class RecFoldable[A]( self : Traversable[A] ) {
    val evals : scm.Buffer[A] = new scm.ArrayBuffer[A]
  }

  implicit val recFoldable : Foldable[RecFoldable] = new Foldable[RecFoldable] {
    override def foldLeft[A, B]( fa : RecFoldable[A], b : B )( f : ( B, A ) => B ) : B = {
      fa.self.foldLeft( b ) { ( b, a ) => fa.evals += a; f( b, a ) }
    }

    override def foldRight[A, B]( fa : RecFoldable[A], lb : Eval[B] )( f : ( A, Eval[B] ) => Eval[B] ) : Eval[B] = {
      fa.self.foldRight( lb ) { ( a, lb ) => fa.evals += a; f( a, lb ) }
    }
  }
}
