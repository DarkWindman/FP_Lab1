import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import CommutativeSemigroupOps._

object CommutativeSemigroupSpesification extends Properties("CommutativeSemigroup") {

        property("associative") = forAll { (a: Option[Int], b: Option[Int], c: Option[Int]) =>
                (a ++ b) ++ c == a ++ (b ++ c)
        }

        property("commutative") = forAll { (a: Option[Int], b: Option[Int]) =>
                a ++ b == b ++ a
        }

        }