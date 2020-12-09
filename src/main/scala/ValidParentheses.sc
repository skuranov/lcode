

object Solution {
  def isValid(s: String): Boolean = {
    def isValidInner(inS: String, expected: String, convolution: String): Boolean = {
      if ("ERROR" == expected) false
      else if (inS.isEmpty && convolution.isEmpty) true
      else if ("ERROR" == convolution) false
      else if (inS.isEmpty && !convolution.isEmpty) false
      else if (expected.nonEmpty && !expected.contains(inS.head)) {
        false
      }
      else {
        isValidInner(inS.tail, getExpected(inS.head, convolution), getConvolution(inS.head, convolution))
      }
    }

    isValidInner(s, "", "")
  }

  def getConvolution(c: Char, current: String): String = {
    if ("{([".contains(c)) current.appended(c)
    else if ("})]".contains(c)) current.init
    else current
  }

  def getExpected(c: Char, convolution: String): String = {
    c match {
      case '{' => "{}[("
      case '(' => "()[{"
      case '[' => "[]({"
      case _ if convolution.isEmpty => "ERROR"
      case _ if convolution.length == 1 => ""
      case _ => getExpected(convolution.init.last, convolution.init)
    }
  }
}

  Solution.isValid("{[]}")