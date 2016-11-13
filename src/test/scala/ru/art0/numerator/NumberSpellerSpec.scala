package ru.art0.numerator

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class NumberSpellerSpec extends FlatSpec with Matchers {

  "makePlural" should "make plural forms for simple cases" in {

    val word = PluralFormedWord("лимон", "лимона", "лимонов")

    val testCases = Map[Int, String](
      1 -> "лимон",
      3 -> "лимона",
      9 -> "лимонов",
      12 -> "лимонов",
      18 -> "лимонов",
      20 -> "лимонов",
      21 -> "лимон",
      22 -> "лимона",
      100 -> "лимонов",
      101 -> "лимон",
      111 -> "лимонов",
      121 -> "лимон",
      199 -> "лимонов",
      300 -> "лимонов",
      301 -> "лимон",
      1001 -> "лимон",
      1002 -> "лимона"
    )

    testCases.toSeq.foreach {
      case (number, expectedSpelling) =>
        NumberSpeller.makePlural(word, number) should be (expectedSpelling)
    }
  }

  "spell" should "spell simple numbers" in {

    val testCases = Map(
      1 -> "один рубль",
      3 -> "три рубля",
      5 -> "пять рублей",
      10 -> "десять рублей",
      13 -> "тринадцать рублей",
      20 -> "двадцать рублей",
      21 -> "двадцать один рубль",
      27 -> "двадцать семь рублей",
      35 -> "тридцать пять рублей",
      90 -> "девяносто рублей",
      99 -> "девяносто девять рублей"
    )

    testCases.toSeq.foreach {
      case (number, expectedSpelling) =>
        NumberSpeller.spell(number, MasculineWord) should be (expectedSpelling)
    }
  }

  it should "spell simple numbers in feminine gender" in {

    val testCases = Map(
      1 -> "одна ложка",
      3 -> "три ложки",
      5 -> "пять ложек",
      11 -> "одиннадцать ложек",
      21 -> "двадцать одна ложка",
      99 -> "девяносто девять ложек"
    )

    testCases.toSeq.foreach {
      case (number, expectedSpelling) =>
        NumberSpeller.spell(number, FeminineWord) should be (expectedSpelling)
    }
  }

  it should "spell simple numbers in neuter gender" in {

    val testCases = Map(
      1 -> "одно облако",
      3 -> "три облака",
      5 -> "пять облаков",
      11 -> "одиннадцать облаков",
      21 -> "двадцать одно облако",
      99 -> "девяносто девять облаков"
    )

    testCases.toSeq.foreach {
      case (number, expectedSpelling) =>
        NumberSpeller.spell(number, NeuterWord) should be (expectedSpelling)
    }
  }

  it should "spell number under thousand" in {
    NumberSpeller.spell(123, MasculineWord) should be (
      "сто двадцать три рубля"
    )
  }

  it should "spell number under thousand in masculine gender" in {
    NumberSpeller.spell(751, MasculineWord) should be (
      "семьсот пятьдесят один рубль"
    )
  }

  it should "spell number under thousand in feminine gender" in {
    NumberSpeller.spell(751, FeminineWord) should be (
      "семьсот пятьдесят одна ложка"
    )
  }

  it should "spell number in thousands only" in {
    NumberSpeller.spell(456000, MasculineWord) should be (
      "четыреста пятьдесят шесть тысяч рублей"
    )
  }

  it should "spell number in millions only" in {
    NumberSpeller.spell(789000000, MasculineWord) should be (
      "семьсот восемьдесят девять миллионов рублей"
    )
  }

  it should "spell number in billions only" in {
    NumberSpeller.spell(42000000000l, MasculineWord) should be (
      "сорок два миллиарда рублей"
    )
  }

  it should "spell zero" in {
    NumberSpeller.spell(0, MasculineWord) should be (
      "ноль рублей"
    )
  }

  it should "spell complex case" in {
    NumberSpeller.spell(12345678901l, MasculineWord) should be (
      "двенадцать миллиардов триста сорок пять миллионов шестьсот семьдесят восемь тысяч девятьсот один рубль"
    )
  }

  private val MasculineWord = Word(PluralFormedWord("рубль", "рубля", "рублей"), Gender.Masculine)
  private val FeminineWord = Word(PluralFormedWord("ложка", "ложки", "ложек"), Gender.Feminine)
  private val NeuterWord = Word(PluralFormedWord("облако", "облака", "облаков"), Gender.Neuter)
}
