package datastructure

class LinkedList(val data: Any) {

  var head = new Node(data)
  var tail = head

  def count(): Int ={
    var cnt = 0
    var cur = head
    while (cur != null) {
      cnt += 1
      cur = cur.next
    }
    return cnt
  }

  def appendToTail(input: Any): Unit ={
    var newNode = new Node(input)
    tail.next = newNode
    tail = newNode
  }

  def appendToHead(input: Any): Unit ={
    var newNode = new Node(input)
    var temp = head
    head = newNode
    head.next = temp
  }

  def printList(): Unit ={

    var cur = head
    while (cur != null) {
      println(cur.data)
      cur = cur.next
    }
  }

  def printListOrder(): Unit ={
    var cur = head
    while (cur != null) {
      print(cur.data)
      cur = cur.next
      if (cur != null) print(" -> ")
    }
  }

  def printListReverse(): Unit ={
    var cur = head
    val list = new LinkedList(cur.data)
    cur = cur.next

    while (cur != null) {
      list.appendToHead(cur.data)
      cur = cur.next
    }
    list.printListOrder()
  }
}
