package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import main.Cube

class CubeSpec extends FlatSpec with ShouldMatchers {

  "A Cube" must "find the smallest cube root with exactly 3 permutations" in {
    val resultExpected = Array[Int](345,384,405)
    val resultReturned = Cube.findSmallestRootWithNPermutations(3, true).get

    resultReturned should equal (resultExpected)
  }

  it must "find the smallest cube root with exactly 2 permutations, starting at 345" in {
    val resultExpected = Array[Int](350,380)
    val resultReturned = Cube.findSmallestRootWithNPermutations(2, true, 345).get

    resultReturned should equal (resultExpected)
  }

  it must "find the smallest cube root with least 2 permutations, starting at 345" in {
    val resultExpected = Array[Int](345,384,405)
    val resultReturned = Cube.findSmallestRootWithNPermutations(2, false, 345).get

    resultReturned should equal (resultExpected)
  }

  it must "find the smallest cubed value with exactly 3 permutations" in {
    val resultExpected = BigDecimal("41063625").toBigInt
    val resultReturned = Cube.findSmallestValueWithNPermutations(3, true).get

    resultReturned should equal (resultExpected)
  }

  ignore must "find the smallest cubed value with exactly 5 permutations" in {
    val resultExpected = BigDecimal("127035954683").toBigInt
    val resultReturned = Cube.findSmallestValueWithNPermutations(5, true).get

    resultReturned should equal (resultExpected)
  }

  it must "generate whole number roots of all permutations from 125" in {
    val resultExpected = Array[Int](5,8)
    val resultReturned = Cube.findAllRootPermutations(5).sorted.toArray

    resultReturned should equal (resultExpected)
  }

  it must "find the smallest cubed value with exactly 3 permutations, but less than 100. Should be None" in {
    val resultExpected = None
    val resultReturned = Cube.findSmallestValueWithNPermutations(3, true, maxIterations = 100)

    resultReturned should equal (resultExpected)
  }

}
