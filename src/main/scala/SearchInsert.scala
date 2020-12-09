object SearchInsert {

  object Solution {
    def searchInsert(nums: Array[Int], target: Int): Int = {
      def searchInsertInner(numsInner: Array[Int], pointer: Int): Int = {
        if (numsInner.isEmpty || numsInner.head >= target) pointer
        else (searchInsertInner(numsInner.tail, pointer + 1))
      }
      searchInsertInner(nums, 0)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.searchInsert(Array(1,3,5,6), 5))
  }
}
