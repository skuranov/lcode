object Solution {
  def longestCommonPrefix(strs: Array[String]): String = {
    val strsList = strs.toList

    def lcpInner(strListInner: List[String], acc: String): String = {
      if (strListInner.nonEmpty && strListInner.forall(x => x.nonEmpty) && areFirstEqual(strListInner))
        lcpInner(strListInner.map(x => x.substring(1)), acc.appended(strListInner.head(0)))
      else acc
    }

    def areFirstEqual(strListInner: List[String]): Boolean = {
      val firstSymbol = strListInner(0).charAt(0)
      strListInner.forall(x => x.charAt(0) == firstSymbol)
    }

    lcpInner(strsList, "")
  }
}

ValidParentheses.longestCommonPrefix(Array("flower", "flow", "flight"))

ValidParentheses.longestCommonPrefix(Array("dog","racecar","car"))

ValidParentheses.longestCommonPrefix(Array())