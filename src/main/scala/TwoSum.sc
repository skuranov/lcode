object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val numList = nums.toList
    def twoSumIn(i1: Int, v1: Int, i2: Int, numsIn: List[Int]): Array[Int] = {
      if (numsIn.nonEmpty && chkSum(v1, numsIn.head)) {
        Array(i1, i2)
      }
      else if (numsIn.tail.isEmpty) {
        twoSumIn(i1 + 1, nums(i1 + 1), i1 + 2, numList.slice(i1 + 2, nums.length))
      }
      else {
        twoSumIn(i1, v1, i2 + 1, numsIn.tail)
      }
    }
    def chkSum(x: Int, y: Int): Boolean = {
      x + y == target
    }
    twoSumIn(0, nums.head, 1, numList.tail)
  }
}

ValidParentheses.twoSum(Array(0,2,4,6,8,10,12,14,16,18,20,22,1021,24,26,28,30,32,34,36,15000),16021)