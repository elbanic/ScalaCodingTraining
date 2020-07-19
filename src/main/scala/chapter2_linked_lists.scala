
import java.lang.Math

import datastructure.{LinkedList, Node}

import scala.collection.mutable.{ArrayBuffer, ListBuffer, HashMap}


class chapter2_linked_lists {

  def run(): Unit = {
    println("===== Chapter 2. =====")
    println("===== Practice 1. =====")
    removeDups()

    println("===== Practice 2. =====")
    kthToLast(3)

    println("===== Practice 3. =====")
    val listDelete = new LinkedList(1)
    listDelete.appendToTail(2)
    listDelete.appendToTail(3)
    listDelete.appendToTail(4)
    listDelete.appendToTail(5)
    deleteMidNode(listDelete,3)

    println("===== Practice 5. =====")
    val list1 = new LinkedList(6)
    list1.appendToTail(1)
    list1.appendToTail(7)

    val list2 = new LinkedList(2)
    list2.appendToTail(9)
    list2.appendToTail(5)

    sumOrder(list1, list2)
    sumReverse(list1, list2)

    println("===== Practice 6. =====")
    val listPalindrome = new LinkedList(1)
    listPalindrome.appendToTail(2)
    listPalindrome.appendToTail(3)
    listPalindrome.appendToTail(2)
    listPalindrome.appendToTail(1)

    println(palindrome(listPalindrome))
    val listPalindrome2 = deleteMidNode(listPalindrome, 3)
    println(palindrome(listPalindrome2))


    println("===== Practice 7. =====")
    val inter1 = new LinkedList(1)
    inter1.appendToTail(2)
    inter1.appendToTail(3)
    val inter2 = new LinkedList(2)
    inter2.appendToTail(3)
    println(intersection(inter1, inter2))

    println("===== Practice 8. =====")
    loopDetection()
  }

  // 1. Remove Dups: Write code to remove duplicates from an unsorted linked list.
  def removeDups(): Unit ={
    val list = new LinkedList(1)
    list.appendToTail(2)
    list.appendToTail(3)
    list.appendToTail(2)
    list.appendToTail(3)
    println("===== current list =====")
    list.printList()

    val tempArr = new ArrayBuffer[Node]
    var cur = list.head
    while (cur != null) {
      tempArr.append(cur)
      cur = cur.next
    }

    println("===== removed dups list =====")
    var distArr = tempArr.distinctBy(f => f.data)
    val list2 = new LinkedList
    for (i <- distArr) {
      list2.appendToTail(i.data)
    }
    list2.printList()
  }

  // 2. Return Kth to Last: Implement an algorithm to find the kth to last element of a singly linked list.
  def kthToLast(k: Int): Unit ={

    val list = new LinkedList(1)
    list.appendToTail(2)
    list.appendToTail(3)
    list.appendToTail(4)
    list.appendToTail(5)

    println("===== current list =====")
    list.printList()

    val tempArr = new ArrayBuffer[Node]
    var cur = list.head
    while (cur != null) {
      tempArr.append(cur)
      cur = cur.next
    }
    println("===== " + k.toString + " th to Last =====")
    println(tempArr(tempArr.size-1 - k).data)

  }


  // 3. Delete Middle Node: Implement an algorithm to delete a node in the middle (i.e., any node but the first and last node,
  // not necessarily the exact middle) of a singly linked list, given only access to that node.
  def deleteMidNode(list: LinkedList, mid: Int): LinkedList ={

    println("===== current list =====")
    list.printList()
    val tempArr = new ArrayBuffer[Node]
    var cur = list.head
    while (cur != null) {
      tempArr.append(cur)
      cur = cur.next
    }

    println("===== delete " + mid + " node =====")
    tempArr.remove(mid)
    val list2 = new LinkedList(tempArr(0).data)
    for (i <- tempArr -= tempArr(0)) {
      list2.appendToTail(i.data)
    }
    list2.printList()
    list2
  }

  // 4. Partition: Write code to partition a linked list around a value x, such that all nodes less than x come before
  // all nodes greater than or equal to x. lf x is contained within the list, the values of x only need to be after
  // the elements less than x (see below). The partition element x can appear anywhere in the "right partition";
  // it does not need to appear between the left and right partitions.

  //EXAMPLE
  //Input: 3 -> 5 -> 8 -> 5 ->10 -> 2 -> 1 [partition=5)
  //Output: 3 -> 1 -> 2 -> 10 -> 5 -> 5 -> 8

  def partition(): Unit = {

  }

  // 5. Sum Lists: You have two numbers represented by a linked list, where each node contains a single digit. The digits
  // are stored in reverse order, such that the 1's digit is at the head of the list. Write a function that adds the
  // two numbers and returns the sum as a linked list.

  //EXAMPLE
  //Input: (7 -> 1 -> 6) + (5 -> 9 -> 2). That is, 617 + 295.
  //Output: 2 -> 1 -> 9. That is, 912.
  //FOLLOW UP
  //Suppose the digits are stored in forward order. Repeat the above problem.
  //Input: (6 -> 1 -> 7) + (2 -> 9 -> 5). That is, 617 + 295.
  //Output: 9 -> 1 -> 2. That is, 912.

  def sumOrder(list1: LinkedList, list2: LinkedList): Unit ={
    print("(")
    list1.printListOrder()
    print(") + (")
    list2.printListOrder()
    print(")")

    var cnt = list1.count() - 1
    var cur = list1.head
    var num1 = 0d
    while (cur != null) {
      num1 += cur.data.asInstanceOf[Int] * Math.pow(10,cnt)
      cnt -= 1
      cur = cur.next
    }

    cnt = list2.count() - 1
    cur = list2.head
    var num2 = 0d
    while (cur != null) {
      num2 += cur.data.asInstanceOf[Int] * Math.pow(10,cnt)
      cnt -= 1
      cur = cur.next
    }
    println(" = " + (num1 + num2))
  }

  def sumReverse(list1: LinkedList, list2: LinkedList): Unit ={
    print("(")
    list1.printListReverse()
    print(") + (")
    list2.printListReverse()
    print(")")

    var cnt = list1.count() - 1
    var cur = list1.head
    var num1 = 0d
    while (cur != null) {
      num1 += cur.data.asInstanceOf[Int] * Math.pow(10,cnt)
      cnt -= 1
      cur = cur.next
    }

    cnt = list2.count() - 1
    cur = list2.head
    var num2 = 0d
    while (cur != null) {
      num2 += cur.data.asInstanceOf[Int] * Math.pow(10,cnt)
      cnt -= 1
      cur = cur.next
    }
    println(" = " + (num1 + num2))
  }

  // 6. Palindrome: Implement a function to check if a linked list is a palindrome.

  def palindrome(list: LinkedList): Boolean ={

    val cnt = list.count()
    val mid = (cnt.toFloat/2).toInt

    val preArray = new ArrayBuffer[Int]
    val postArray = new ArrayBuffer[Int]

    // 1,2,3,3,2,1
    if (cnt % 2 == 0) {   //even
      var cur = list.head
      for (i <- 0 to mid) {
        preArray.append(cur.data.asInstanceOf[Int])
        cur = cur.next
      }
      for (i <- mid+1 until cnt) {
        postArray.append(cur.data.asInstanceOf[Int])
        cur = cur.next
      }
      return preArray == postArray.reverse
    }
    // 1,2,3,2,1
    else {    //odd
      var cur = list.head
      for (i <- 0 to mid) {
        preArray.append(cur.data.asInstanceOf[Int])
        cur = cur.next
      }
      for (i <- mid+1 until cnt) {
        postArray.append(cur.data.asInstanceOf[Int])
        cur = cur.next
      }
      return (preArray-=preArray.last) == postArray.reverse
    }
  }

  // 7. Intersection: Given two (singly) linked lists, determine if the two lists intersect. Return the intersecting node.
  // Note that the intersection is defined based on reference, not value. That is, if the kth node of the first linked list is
  // the exact same node (by reference) as the jth node of the second linked list, then they are intersecting.

  def intersection(list1: LinkedList, list2: LinkedList): ArrayBuffer[Int] ={

    val tempArr = new ArrayBuffer[Int]
    var cur = list1.head
    while (cur != null) {
      tempArr.append(cur.data.asInstanceOf[Int])
      cur = cur.next
    }

    val tempArr2 = new ArrayBuffer[Int]
    cur = list2.head
    while (cur != null) {
      tempArr2.append(cur.data.asInstanceOf[Int])
      cur = cur.next
    }

    val intersect = new ArrayBuffer[Int]
    var started = false

    for (i <- tempArr2) {
      if (tempArr.contains(i)) {
        if (intersect.isEmpty) {
          started = true
          intersect.append(i)
        } else {
          if (started)
            intersect.append(i)
        }
      } else {
        started = false
      }
    }
    return intersect
  }

  // 8. Loop Detection: Given a circular linked list, implement an algorithm that returns the node at the beginning of the loop.

  //EXAMPLE
  //Input: A -) B -) C -) 0 -) E -) C [the same C as earlier]
  //Output: C

  def loopDetection(): Unit ={

    val list = List("A", "B", "C", "D", "E", "C")
    val dict = list.groupBy(identity).mapValues(_.size)

    println(dict.filter(i => i._2 > 1).keySet.head)
  }

}
