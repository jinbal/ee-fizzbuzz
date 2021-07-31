package com.equalexperts.fb

import com.equalexperts.fb.NumberConverters._

object FizzBuzz {


  def StepOneConvertToFizzBuzz(numbers: Seq[Int]): String = {
    val converted = numbers.map(runStepOneConversionChain)
    val aggregated = converted.reduceLeft((a, b) => a.append(b))
    aggregated.output
  }

  def StepTwoConvertToFizzBuzz(numbers: Seq[Int]):String = {
    val converted = numbers.map(runStepTwoConversionChain)
    val aggregated = converted.reduceLeft((a, b) => a.append(b))
    aggregated.output
  }

  def StepThreeConvertToFizzBuzz(numbers: Seq[Int]):String = {
    val converted = numbers.map(runStepTwoConversionChain)
    val aggregated = converted.reduceLeft((a, b) => a.append(b))
    aggregated.outputWithStats
  }
}
