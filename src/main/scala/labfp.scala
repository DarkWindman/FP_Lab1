import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty
import org.scalacheck.{Arbitrary, Prop, Shrink}


trait CommutativeSemigroup[A] {
  def combine(l: A, r: A): A
}

object CommutativeSemigroup
{
  def laws[A:Arbitrary: Shrink](implicit sg: CommutativeSemigroup[A], p: A => Pretty):List[Prop]={
    List(
      forAll { (a: A, b: A, c: A) => sg.combine(sg.combine(a,b),c) == sg.combine(a, sg.combine(b, c)) },
      forAll { (a: A, b: A) => sg.combine(a, b) == sg.combine(b, a)}
    )
  }
  implicit val asgOption = new CommutativeSemigroup[Option[Int]] {

    def reduce(v1: Option[Int], v2: Option[Int]): Option[Int] =
      v1.flatMap( x => v2.map(y => x + y).orElse(v1))

    override def combine(l: Option[Int], r: Option[Int]): Option[Int] = {
      if(l.isDefined)
        reduce(l, r)
      else
        reduce(r, l)
    }
  }
}

object labfp {
  def main(args: Array[String]): Unit = {
    CommutativeSemigroup.laws[Option[Int]].foreach(_.check())
  }
}
