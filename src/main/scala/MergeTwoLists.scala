
object MergeTwoLists {
  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {

    def mergeInner(lIn1: ListNode, lIn2: ListNode): Unit = {
      if (lIn2 != null && lIn1 != null) {
        val neighbours = findNeighbours(lIn2.x, lIn1)
        if (neighbours._1 != null) {
          neighbours._1.next = lIn2
          val lIn2Next = lIn2.next
          lIn2.next = neighbours._2
          mergeInner(neighbours._1, lIn2Next)
        }
        else getLast(lIn2).next = neighbours._2
      }
      def getLast(l: ListNode):ListNode = {
        if (l.next == null) l
        else getLast(l.next)
      }
    }

    def findNeighbours(v: Int, node: ListNode): (ListNode, ListNode) = {
      if (node == null) (null, null)
      else if (node.x > v) (null, node)
      else if (node.x <= v && node.next != null && node.next.x >= v) (node, node.next)
      else if (node.next == null) (node, null)
      else findNeighbours(v, node.next)
    }

    def getLonger(l1: ListNode, l2: ListNode): ListNode = {
      def getLength(l: ListNode, acc: Int): Int = {
        if (l.next == null) acc + 1
        else getLength(l.next, acc + 1)
      }

      if (getLength(l1, 0) > getLength(l2, 0)) l1
      else l2
    }

    if (l1 == null) l2
    else if (l2 == null) l1
    else {
      mergeInner(l1, l2)
      getLonger(l1, l2)
    }
  }

  def main(args: Array[String]): Unit = {
    val node1 = new ListNode(1)
    node1.next = new ListNode(2)
    node1.next.next = new ListNode(4)
    val node2 = new ListNode(5)
    println(mergeTwoLists(node2, node1))

    val node3 = null
    val node4 = new ListNode(0)
    println(mergeTwoLists(node3, node4))

    val node5 = new ListNode(2)
    val node6 = new ListNode(1)
    println(mergeTwoLists(node5, node6))
  }

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x

    override def toString: String = {
      if (next != null) x + next.toString
      else x.toString
    }

  }

}
