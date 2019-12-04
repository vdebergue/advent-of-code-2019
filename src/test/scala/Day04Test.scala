import org.scalatest.Matchers._
import org.scalatest._

class Day04Test extends FunSuite {

  test("should match easy numbers") {
    Day04.meetEasyCriterias(111111) should equal (true)
    Day04.meetEasyCriterias(223450) should equal (false)
    Day04.meetEasyCriterias(123789) should equal (false)

    Day04.meetHardCriterias(112233) should equal (true)
    Day04.meetHardCriterias(123444) should equal (false)
    Day04.meetHardCriterias(111122) should equal (true)
  }


}