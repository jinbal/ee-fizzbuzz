package com.equalexperts.fb

object NumberConverters {
  lazy val converterChain = Seq(FizzConverter, BuzzConverter, FizzBuzzConverter)

  case class ConversionResult(output: String = "", converted: Boolean = false) {
    def append(conversionResult: ConversionResult): ConversionResult = {
      ConversionResult(s"$output ${conversionResult.output}")
    }
  }

  object DefaultConversionResult {
    def apply(num: Int): ConversionResult = ConversionResult(num.toString)
  }

  sealed trait NumberConverter {
    def convert(conversionResult: ConversionResult, numToConvert: Int): ConversionResult
  }

  case object FizzConverter extends NumberConverter {
    override def convert(conversionResult: ConversionResult, numToConvert: Int): ConversionResult = {
      if (!conversionResult.converted && numToConvert % 3 == 0) ConversionResult("fizz", true) else conversionResult
    }
  }

  case object BuzzConverter extends NumberConverter {
    override def convert(conversionResult: ConversionResult, numToConvert: Int): ConversionResult = {
      if (!conversionResult.converted && numToConvert % 5 == 0) ConversionResult("buzz", true) else conversionResult
    }
  }

  case object FizzBuzzConverter extends NumberConverter {
    override def convert(conversionResult: ConversionResult, numToConvert: Int): ConversionResult = {
      if (numToConvert % 15 == 0) ConversionResult("fizzbuzz", true) else conversionResult
    }
  }

  def runConversionChain(numToConvert: Int): ConversionResult = {
    converterChain.foldLeft(DefaultConversionResult(numToConvert)) { case (res, converter) =>
      converter.convert(res, numToConvert)
    }
  }
}
