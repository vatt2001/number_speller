package ru.art0.numerator

object NumberSpeller {

  /**
    * Spell number and add unit
    * @param number Number to spell
    * @param unitWord unit for the number
    * @return
    */
  def spell(number: Long, unitWord: Word): String = {
    if (number >= 0) {
      (ThousandGroups ++ Seq(ThousandGroup(1, unitWord)))
        .flatMap(spellThousandGroup(number, _))
        .mkString(Separator)
    } else {
      throw new RuntimeException("Only positive numbers can be spelled")
    }
  }

  /**
    * Chooses proper plural form according to number
    * @param pluralForms Different plural forms of the word
    * @param number Number to choose plural form for
    * @return
    */
  def makePlural(pluralForms: PluralFormedWord, number: Long): String = {
    if (tenToTwenty.contains(number % 100)) {
      pluralForms.plural
    } else {
      number % 10 match {
        case 1 => pluralForms.single
        case 2 | 3 | 4 => pluralForms.dual
        case _ => pluralForms.plural
      }
    }
  }

  // internal

  private def spellThousandGroup(number: Long, group: ThousandGroup): Option[String] = {
    val numberUnderThousand = number / group.baseNumber % 1000
    if (numberUnderThousand > 0) {
      Some(
        spellUnderThousand(numberUnderThousand, group.word.gender) +
          Separator +
          makePlural(group.word.pluralForms, numberUnderThousand)
      )
    } else if (number == 0 && group.baseNumber == 1) {
      Some(
        spellZero(group.word)
      )
    } else if (numberUnderThousand == 0 && group.baseNumber == 1) {
      // The last group part of number should be spelled anyway even if it is 0
      Some(
        makePlural(group.word.pluralForms, numberUnderThousand)
      )
    } else {
      None
    }
  }

  private def spellUnderThousand(number: Long, gender: Gender.Value): String = {
    var parts = List.empty[Long]

    if (tenToTwenty.contains(number % 100)) {
      parts ::= number % 100 // 10..19
    } else {
      parts ::= number % 10 // units
      parts ::= number / 10 % 10 * 10 // tens
    }
    parts ::= number / 100 % 10 * 100 // hundreds

    parts.filterNot(_ == 0)
      .flatMap(n => SpelledNumbers.get(n))
      .map(_.toGenderedString(gender))
      .mkString(Separator)
  }

  private def spellZero(word: Word): String = {
    SpelledNumbers(0).toGenderedString(word.gender) +
      Separator +
      makePlural(word.pluralForms, 0)
  }

  case class ThousandGroup(baseNumber: Long, word: Word)

  private val tenToTwenty = 10 to 20

  private val Separator = " "

  private val ThousandGroups = List(
    ThousandGroup(1000000000, Word(PluralFormedWord("миллиард", "миллиарда", "миллиардов"), Gender.Masculine)),
    ThousandGroup(1000000, Word(PluralFormedWord("миллион", "миллиона", "миллионов"), Gender.Masculine)),
    ThousandGroup(1000, Word(PluralFormedWord("тысяча", "тысячи", "тысяч"), Gender.Feminine))
  )

  private val SpelledNumbers = Map[Long, GenderedWord](
    0l -> DeclensionalWord("ноль", "ноль", "ноль"),
    1l -> DeclensionalWord("один", "одна", "одно"),
    2l -> DeclensionalWord("два", "две", "два"),
    3l -> SingleFormWord("три"),
    4l -> SingleFormWord("четыре"),
    5l -> SingleFormWord("пять"),
    6l -> SingleFormWord("шесть"),
    7l -> SingleFormWord("семь"),
    8l -> SingleFormWord("восемь"),
    9l -> SingleFormWord("девять"),
    10l -> SingleFormWord("десять"),
    11l -> SingleFormWord("одиннадцать"),
    12l -> SingleFormWord("двенадцать"),
    13l -> SingleFormWord("тринадцать"),
    14l -> SingleFormWord("четырнадцать"),
    15l -> SingleFormWord("пятнадцать"),
    16l -> SingleFormWord("шестнадцать"),
    17l -> SingleFormWord("семнадцать"),
    18l -> SingleFormWord("восемнадцать"),
    19l -> SingleFormWord("девятнадцать"),
    20l -> SingleFormWord("двадцать"),
    30l -> SingleFormWord("тридцать"),
    40l -> SingleFormWord("сорок"),
    50l -> SingleFormWord("пятьдесят"),
    60l -> SingleFormWord("шестьдесят"),
    70l -> SingleFormWord("семьдесят"),
    80l -> SingleFormWord("восемьдесят"),
    90l -> SingleFormWord("девяносто"),
    100l -> SingleFormWord("сто"),
    200l -> SingleFormWord("двести"),
    300l -> SingleFormWord("триста"),
    400l -> SingleFormWord("четыреста"),
    500l -> SingleFormWord("пятьсот"),
    600l -> SingleFormWord("шестьсот"),
    700l -> SingleFormWord("семьсот"),
    800l -> SingleFormWord("восемьсот"),
    900l -> SingleFormWord("девятьсот")
  )
}

object Gender extends Enumeration {
  val Masculine = Value
  val Feminine = Value
  val Neuter = Value
}

case class PluralFormedWord(single: String, dual: String, plural: String)

abstract class GenderedWord(val masculineForm: String, val feminineForm: String, neuterForm: String) {
  def toGenderedString(genus: Gender.Value): String = genus match {
    case Gender.Masculine => masculineForm
    case Gender.Feminine => feminineForm
    case Gender.Neuter => neuterForm
  }
}
case class SingleFormWord(value: String) extends GenderedWord(value, value, value)
case class DeclensionalWord(m: String, f: String, n: String) extends GenderedWord(m, f, n)

case class Word(pluralForms: PluralFormedWord, gender: Gender.Value)