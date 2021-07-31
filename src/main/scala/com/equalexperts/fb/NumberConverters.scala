package com.equalexperts.fb

import scala.annotation.tailrec

object NumberConverters {
  lazy val stepOneConverterChain = Seq(FizzConverter, BuzzConverter, FizzBuzzConverter)
  lazy val stepTwoConverterChain = Seq(FizzConverter, BuzzConverter, FizzBuzzConverter, LuckyConverter)

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

  case object LuckyConverter extends NumberConverter {
    @tailrec
    private def containsDigit(numToCheck: Int, digit: Int): Boolean = {
      numToCheck match {
        case 0 => false
        case c if c % 10 == digit => true
        case _ => containsDigit(numToCheck / 10, digit)
      }
    }

    override def convert(conversionResult: ConversionResult, numToConvert: Int): ConversionResult = {
      if(containsDigit(numToConvert, 3)) ConversionResult("lucky", true) else conversionResult
    }
  }

  def runStepOneConversionChain(numToConvert: Int): ConversionResult = {
    stepOneConverterChain.foldLeft(DefaultConversionResult(numToConvert)) { case (res, converter) =>
      converter.convert(res, numToConvert)
    }
  }

  def runStepTwoConversionChain(numToConvert: Int): ConversionResult = {
    stepTwoConverterChain.foldLeft(DefaultConversionResult(numToConvert)) { case (res, converter) =>
      converter.convert(res, numToConvert)
    }
  }
}
