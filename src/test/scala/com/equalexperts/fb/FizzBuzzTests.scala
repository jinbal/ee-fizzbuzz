package com.equalexperts.fb

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class FizzBuzzTests extends AnyFunSuite with Matchers {

  test("Step One: FizzBuzz should convert a contiguous list of integer to fizzbuzz") {
    // Given
    val range  = (1 to 20).toList

    // When
    val result = FizzBuzz.StepOneConvertToFizzBuzz(range)

    // Then
    result shouldBe "1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz 16 17 fizz 19 buzz"
  }

  test("Step Two: FizzBuzz should convert to FizzBuzz with lucky for numbers containing the digit 3") {
    // Given
    val range  = (1 to 20).toList

    // When
    val result = FizzBuzz.StepTwoConvertToFizzBuzz(range)

    // Then
    result shouldBe "1 2 lucky 4 buzz fizz 7 8 fizz buzz 11 fizz lucky 14 fizzbuzz 16 17 fizz 19 buzz"
  }

  test("Step Three: FizzBuzz should convert to FizzBuzz and Lucky with conversion stats") {
    // Given
    val range  = (1 to 20).toList

    // When
    val result = FizzBuzz.StepThreeConvertToFizzBuzz(range)

    // Then
    result shouldBe "1 2 lucky 4 buzz fizz 7 8 fizz buzz 11 fizz lucky 14 fizzbuzz 16 17 fizz 19 buzz fizz: 4 buzz: 3 fizzbuzz: 1 lucky: 2 integer: 10"

  }

}
