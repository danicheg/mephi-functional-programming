import PrettyNumbers._
import org.specs2.mutable.Specification

class PrettyNumbersSpec extends Specification {

    "Function getNaturalNumbers" should {
        "works correct in special case" in {
            getNaturalNumbers(0).toList mustEqual List.empty
        }

        "generate correct range of integers" in {
            getNaturalNumbers(10).toList mustEqual List(1,2,3,4,5,6,7,8,9,10)
        }
    }

    "Function getTriangularNumbers" should {
        "works correct in special case" in {
            getTriangularNumbers(0) mustEqual List.empty
        }

        "generate correct list of triangular number" in {
            getTriangularNumbers(36) mustEqual List(0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55,
                66, 78, 91, 105, 120, 136, 153, 171, 190, 210, 231, 253, 276, 300, 325, 351,
                378, 406, 435, 465, 496, 528, 561, 595, 630, 666)
        }
    }

    "Function getPyramidalNumbers" should {
        "works correct in special case" in {
            getPyramidalNumbers(0) mustEqual List.empty
        }

        "generate correct list of pyramidal number" in {
            getPyramidalNumbers(32) mustEqual List(	0, 1, 5, 14, 30, 55, 91, 140,
                204, 285, 385, 506, 650, 819, 1015, 1240, 1496, 1785, 2109, 2470, 2870, 3311,
                3795, 4324, 4900, 5525, 6201, 6930, 7714, 8555, 9455, 10416, 11440)
        }
    }

}
