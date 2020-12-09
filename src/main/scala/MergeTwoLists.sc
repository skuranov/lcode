class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x

  override def toString: String = x + next.toString
}

object Solution {
  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {

    def  mergeInner(l1: ListNode, l2: ListNode, acc:ListNode) : ListNode = {
      if (l1 == null && l2 == null) acc
      else if (acc._x <= l1._x) {
        acc.next = l1
        mergeInner(l1.next, l2, l1)
      }
      else {
        acc.next = l2
        mergeInner(l1, l2.next, l2)
      }
    }

    mergeInner(l1,l2, new ListNode)
  }
}

println(Solution.mergeTwoLists(new ListNode(0), new ListNode(1)))