import scala.language.implicitConversions


trait CommutativeSemigroup[A] {
  def combine(l: A, r: A): A
}

object CommutativeSemigroup
{
  def reduce(v1: Option[Int], v2: Option[Int]): Option[Int] =
    v1.flatMap( x => v2.map(y => x + y).orElse(v1))
  implicit val OptionIntCommutativeSemigroup: CommutativeSemigroup[Option[Int]] =
  new CommutativeSemigroup[Option[Int]] {
    override def combine(l: Option[Int], r: Option[Int]): Option[Int] = {
      if(l.isDefined)
        reduce(l, r)
      else
        reduce(r, l)
    }
  }
  }
class CommutativeSemigroupOps[A](a: A)(implicit g: CommutativeSemigroup[A]) {
  def ++(b:A): A = g.combine(a, b)
}

object CommutativeSemigroupOps {
  implicit def commutativeSemigroupSyntax[A](a: A)(implicit g: CommutativeSemigroup[A]): CommutativeSemigroupOps[A] = {
    new CommutativeSemigroupOps[A](a)
  }

}
