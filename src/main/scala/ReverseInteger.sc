object Solution {
  def reverse(x: Int): Int = {
    def negIfAppl(target: Int): Int = {
      if (x >= 0) target
      else -target
    }
    val longVal = x.toString.replace("-", "").reverse.toLong
    if (longVal > Int.MaxValue)
      0
    else
      negIfAppl(longVal.toInt)
  }
}

ValidParentheses.reverse(1534236469)


ValidParentheses.reverse(-45646374)