package com.equalexperts.fb

import com.equalexperts.fb.NumberConverters.runConversionChain

object FizzBuzz {

  def convertToFizzBuzz(numbers: Seq[Int]): String = {
    val converted = numbers.map(runConversionChain)
    val aggregated = converted.reduceLeft((a, b) => a.append(b))
    aggregated.output
  }

}
