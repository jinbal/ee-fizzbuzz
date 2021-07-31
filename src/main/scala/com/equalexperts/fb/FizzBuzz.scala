package com.equalexperts.fb

import com.equalexperts.fb.NumberConverters.runStepOneConversionChain

object FizzBuzz {

  def StepOneConvertToFizzBuzz(numbers: Seq[Int]): String = {
    val converted = numbers.map(runStepOneConversionChain)
    val aggregated = converted.reduceLeft((a, b) => a.append(b))
    aggregated.output
  }

}
