import common.{Matrix, Point3D}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
class Point3D extends AnyFunSuite with Matchers {

  test("rotateAroundPoint") {

    val rotateCenter = Point3D(0,-1,0)
    //around D axis
    Point3D(0,1,0).rotateAroundPoint(rotateCenter, 0,0,1) shouldBe Point3D(-2,-1,0)
    Point3D(0,1,0).rotateAroundPoint(rotateCenter, 0,0,2) shouldBe Point3D(0,-3,0)
    Point3D(0,1,0).rotateAroundPoint(rotateCenter, 0,0,3) shouldBe Point3D(2,-1,0)

    // along L axis
    Point3D(0,1,0).rotateAroundPoint(Point3D.zero, 1,0,0) shouldBe Point3D(0,0,1)
    Point3D(0,1,0).rotateAroundPoint(Point3D.zero, 2,0,0) shouldBe Point3D(0,-1,0)
    Point3D(0,1,0).rotateAroundPoint(Point3D.zero, 3,0,0) shouldBe Point3D(0,0,-1)

    // along C axis
    Point3D(1,0,0).rotateAroundPoint(Point3D.zero, 0,1,0) shouldBe Point3D(0,0,-1)
    Point3D(1,0,0).rotateAroundPoint(Point3D.zero, 0,2,0) shouldBe Point3D(-1,0,0)
    Point3D(1,0,0).rotateAroundPoint(Point3D.zero, 0,3,0) shouldBe Point3D(0,0,1)
  }

}
