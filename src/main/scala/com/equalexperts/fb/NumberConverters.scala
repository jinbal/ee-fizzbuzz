package com.equalexperts.fb

import scala.annotation.tailrec

object NumberConverters {
  lazy val stepOneConverterChain = Seq(FizzConverter, BuzzConverter, FizzBuzzConverter)
  lazy val stepTwoConverterChain = Seq(FizzConverter, BuzzConverter, FizzBuzzConverter, LuckyConverter)

  case class ConversionResult(output: String = "",
                              converted: Boolean = false,
                              conversionStats: ConversionStats = DefaultConversionStats()) {
    def append(conversionResult: ConversionResult): ConversionResult = {
      ConversionResult(
        output = s"$output ${conversionResult.output}",
        conversionStats = conversionStats.append(conversionResult.conversionStats)
      )
    }

    def outputWithStats: String = s"$output ${conversionStats.output}"
  }

  case class ConversionStats(fizzCount: Int = 0,
                             buzzCount: Int = 0,
                             fizzBuzzCount: Int = 0,
                             luckyCount: Int = 0,
                             unconvertedCount: Int = 0) {
    def append(conversionStats: ConversionStats): ConversionStats = {
      ConversionStats(
        fizzCount + conversionStats.fizzCount,
        buzzCount + conversionStats.buzzCount,
        fizzBuzzCount + conversionStats.fizzBuzzCount,
        luckyCount + conversionStats.luckyCount,
        unconvertedCount + conversionStats.unconvertedCount
      )
    }

    def output: String = {
      s"fizz: $fizzCount buzz: $buzzCount fizzbuzz: $fizzBuzzCount lucky: $luckyCount integer: $unconvertedCount"
    }
  }

  object DefaultConversionStats {
    def apply(): ConversionStats = ConversionStats(unconvertedCount = 1)
  }

  object DefaultConversionResult {
    def apply(num: Int): ConversionResult = ConversionResult(output = num.toString, conversionStats = DefaultConversionStats())
  }

  sealed trait NumberConverter {
    def convert(conversionResult: ConversionResult, numToConvert: Int): ConversionResult
  }

  case object FizzConverter extends NumberConverter {
    override def convert(conversionResult: ConversionResult, numToConvert: Int): ConversionResult = {
      if (!conversionResult.converted && numToConvert % 3 == 0) ConversionResult("fizz", true, ConversionStats(fizzCount = 1)) else conversionResult
    }
  }

  case object BuzzConverter extends NumberConverter {
    override def convert(conversionResult: ConversionResult, numToConvert: Int): ConversionResult = {
      if (!conversionResult.converted && numToConvert % 5 == 0) ConversionResult("buzz", true, ConversionStats(buzzCount = 1)) else conversionResult
    }
  }

  case object FizzBuzzConverter extends NumberConverter {
    override def convert(conversionResult: ConversionResult, numToConvert: Int): ConversionResult = {
      if (numToConvert % 15 == 0) ConversionResult("fizzbuzz", true, ConversionStats(fizzBuzzCount = 1)) else conversionResult
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
      if (containsDigit(numToConvert, 3)) ConversionResult("lucky", true, ConversionStats(luckyCount = 1)) else conversionResult
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
