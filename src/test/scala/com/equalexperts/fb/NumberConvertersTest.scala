package com.equalexperts.fb

import com.equalexperts.fb.NumberConverters._
import org.scalatest.Inspectors
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class NumberConvertersTest extends AnyFunSuite with Matchers {

  test("Fizz converter should convert multiples of 3") {
    // Given
    val converter = FizzConverter
    val multiplesOf3 = Seq(3, 6, 9, 12, 15, 18, 21)
    // When
    val result = multiplesOf3.map(n => converter.convert(DefaultConversionResult(n), n))

    // Then
    Inspectors.forAll(result) { res =>
      res.output shouldBe "fizz"
      res.converted shouldBe true
    }
  }

  test("Fizz converter should ignore non multiples of 3") {
    // Given
    val converter = FizzConverter
    val invalidNumbers = Seq(2, 5, 8)

    // When
    val result = invalidNumbers.map(n => converter.convert(DefaultConversionResult(n), n))

    // Then
    Inspectors.forAll(result.zip(invalidNumbers)) { case (res, invNum) =>
      res.output shouldBe invNum.toString
      res.converted shouldBe false
    }
  }

  test("Buzz converter should convert multiples of 5") {
    // Given
    val converter = BuzzConverter
    val multiplesOf5 = Seq(5, 10, 15, 20, 25, 30, 35, 40)

    // When
    val result = multiplesOf5.map(n => converter.convert(DefaultConversionResult(n), n))

    // Then
    Inspectors.forAll(result) { res =>
      res.output shouldBe "buzz"
      res.converted shouldBe true
    }
  }

  test("Buzz converter should ignore non multiples of 5") {
    // Given
    val converter = BuzzConverter
    val invalidNumbers = Seq(2, 6, 8, 12, 18)

    // When
    val result = invalidNumbers.map(n => converter.convert(DefaultConversionResult(n), n))

    // Then
    Inspectors.forAll(result.zip(invalidNumbers)) { case (res, invNum) =>
      res.output shouldBe invNum.toString
      res.converted shouldBe false
    }
  }


  test("FizzBuzz converter should convert multiples of 15") {
    // Given
    val converter = FizzBuzzConverter
    val multiplesOf15 = Seq(15, 30, 45, 60, 75, 90)

    // When
    val result = multiplesOf15.map(n => converter.convert(DefaultConversionResult(n), n))

    // Then
    Inspectors.forAll(result) { res =>
      res.output shouldBe "fizzbuzz"
      res.converted shouldBe true
    }
  }

  test("Fizz converter should ignore non multiples of 15") {
    // Given
    val converter = FizzBuzzConverter
    val invalidNumbers = Seq(2, 6, 8, 12, 18)

    // When
    val result = invalidNumbers.map(n => converter.convert(DefaultConversionResult(n), n))

    // Then
    Inspectors.forAll(result.zip(invalidNumbers)) { case (res, invNum) =>
      res.output shouldBe invNum.toString
      res.converted shouldBe false
    }
  }


  test("applying converter chain single number should give correct results") {
    // Given
    val fizzBuzzNumbers = Seq(3, 5, 15)
    val expectedResult = Seq("fizz", "buzz", "fizzbuzz")
    // When
    val result = fizzBuzzNumbers.map(runConversionChain)

    // Then
    Inspectors.forAll(result.zip(expectedResult)) { case (res, str) =>
      res.output shouldBe str
      res.converted shouldBe true
    }
  }
}
