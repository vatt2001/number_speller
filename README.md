## NumberSpeller

Object `NumberSpeller` is intended for spelling numbers in Russian language.

For example:

```
val rub = Word(PluralFormedWord("рубль", "рубля", "рублей"), Gender.Masculine)

NumberSpeller.spell(12345678901l, rub) should be (
  "двенадцать миллиардов триста сорок пять миллионов шестьсот семьдесят восемь тысяч девятьсот один рубль"
)
```