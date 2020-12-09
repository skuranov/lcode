object StrStr {
  object Solution {
    def strStr(haystack: String, needle: String): Int = {
      def strStrInner(innerStack: String, pos: Int): Int = {
        if (innerStack.startsWith(needle))pos
        else if (innerStack.isEmpty)-1
        else strStrInner(innerStack.tail, pos + 1)
      }
      strStrInner(haystack,0)
    }
  }

  def main(args: Array[String]): Unit = {
   println( Solution.strStr("aaaaa", "bba"))
  }
}
