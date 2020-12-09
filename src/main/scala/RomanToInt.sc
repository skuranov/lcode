object Solution {
  def romanToInt(s: String): Int = {
    def romanToIntInner(revStr: String, acc: String, tempInt: Int): Int = {
      if (revStr.isEmpty && acc.isEmpty) tempInt
      else if (!revStr.isEmpty && isConnectable(acc, revStr.head)) romanToIntInner(revStr.tail, acc.appended(revStr.head), tempInt)
      else romanToIntInner(revStr, "", tempInt + subSum(acc))
    }

    def isConnectable(c1: String, c2: Char): Boolean = {
      c1 match {
        case "" => true
        case "V" if c2 == 'I' => true
        case "X" if c2 == 'I' => true
        case "L" if c2 == 'X' => true
        case "C" if c2 == 'X' => true
        case "D" if c2 == 'C' => true
        case "M" if c2 == 'C' => true
        case _ => false
      }
    }

    def subSum(c1: String): Int = {
      c1 match {
        case "I" => 1
        case "VI" => 4
        case "V" => 5
        case "XI" => 9
        case "X" => 10
        case "LX" => 40
        case "L" => 50
        case "CX" => 90
        case "C" => 100
        case "DC" => 400
        case "D" => 500
        case "MC" => 900
        case "M" => 1000
        case _ =>
          println(c1)
          0
      }
    }

    romanToIntInner(s.reverse, "", 0)
  }
}

ValidParentheses.romanToInt("III")
ValidParentheses.romanToInt("MCMXCIV")