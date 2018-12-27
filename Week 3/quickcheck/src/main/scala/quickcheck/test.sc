import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.{const, oneOf}

object test {
  lazy val genMap: Gen[Map[Int,Int]] = oneOf(
    const(Map.empty[Int,Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int,Int]), genMap)
    } yield m.updated(k, v)
  )

  genMap
  genMap
  genMap
  genMap
  genMap
}