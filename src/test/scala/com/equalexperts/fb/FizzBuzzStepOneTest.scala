package com.equalexperts.fb

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class FizzBuzzStepOneTest extends AnyFunSuite with Matchers {

  test("FizzBuzz should convert a contiguous list of integer to fizzbuzz") {
    // Given
    val range  = (1 to 20).toList

    // When
    val result = FizzBuzz.convertToFizzBuzz(range)

    // Then
    result shouldBe "1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz 16 17 fizz 19 buzz"
  }

}
