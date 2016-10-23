import Atom._
import org.specs2.mutable.Specification

class AtomSpec extends Specification {

    "Function set" should {

        "works correct when list is empty" in {
            set(List.empty) mustEqual List.empty
        }

        "works correct in base case" in {
            set(List("a", "a", "b", "c", "c", "a", "z", "b")) mustEqual List("a", "b", "c", "z")
        }

    }

    "Function freq" should {

        "works correct when list is empty" in {
            freq(List.empty) mustEqual List.empty
        }

        "works correct in base case" in {
            freq(List("a", "a", "b", "c", "c", "a", "z", "b")) mustEqual List(("a", 3), ("b", 2), ("c", 2), ("z", 1))
        }
    }

}
