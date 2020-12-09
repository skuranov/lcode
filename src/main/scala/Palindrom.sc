object Solution {
  def isPalindrome(x: Int): Boolean = {
    def ifPalindromeIn(in: Int, out: Int): Boolean = {
      if (in / 10 == 0) {
        out*10 + in % 10 == x
      }
      else ifPalindromeIn(in / 10, out*10 + in % 10)
    }
    if (x < 0) false
    else {
      ifPalindromeIn(x, 0)
    }
  }
}

ValidParentheses.isPalindrome(123)

ValidParentheses.isPalindrome(121)

ValidParentheses.isPalindrome(123321)

ValidParentheses.isPalindrome(-123321)