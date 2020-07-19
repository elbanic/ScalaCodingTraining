

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class chapter1_array_and_strings {

  def run(): Unit = {
    println("===== Chapter 1. =====")
    println("===== Practice 1. =====")
    println(isUnique("abcde"))
    println(isUnique("aabbcccde"))

    println("===== Practice 2. =====")
    println(permutation("abcde", "ebcad"))
    println(permutation("abcde", "ebc"))
    println(permutation("abcde", "ebcaf"))

    println("===== Practice 3. =====")
    println(urlify("Mr John Smith     "))

    println("===== Practice 4. =====")
    println(palindromePermutation("Tact Coa"))
    println(palindromePermutation("act tca"))

    println("===== Practice 5. =====")
    println(oneAway("pale", "ple"))
    println(oneAway("pales", "pale"))
    println(oneAway("pale", "bale"))
    println(oneAway("pale", "bake"))

    println("===== Practice 6. =====")
    println(stringCompression("aabcccccaaa"))

    println("===== Practice 7. =====")

    val mat = Array.ofDim[Int](4,3)
    var values = 0
    for (i <- mat) {
      for (j <- i.indices) {
        i(j) = values
        values += 1
      }
    }
    mat.foreach(i => println(i.mkString(" ")))
    val rotMat = rotateMatrix90(mat)
    rotMat.foreach(i => println(i.mkString(" ")))

    println("===== Practice 8. =====")
    val matZero = Array.ofDim[Int](3,3)
    for (i <- matZero) {
      for (j <- i.indices) {
        i(j) = values
        values += 1
      }
    }
    matZero.foreach(i => println(i.mkString(" ")))
    zeroMatrix(matZero)
    matZero.foreach(i => println(i.mkString(" ")))

    println("===== Practice 9. =====")
    println(isStringRotation("waterbottle", "erbottlewat"))
  }

  // 1. Is Unique: Implement an algorithm to determine if a string has all unique characters.
  // What if you cannot use additional data structures?
  def isUnique(str: String): Boolean = {
    for (i <- str) {
      if ((str.toArray.toBuffer --= Array(i)).mkString.contains(i)) return true
    }
    false
  }

  // 2. Check Permutation: Given two strings, write a method to decide if one is a permutation of the other.
  def permutation(str1: String, str2: String): Boolean = {
    if (str1.size != str2.size) return false
    for (i <- str2) {
      if (!str1.contains(i)) return false
    }
    true
  }

  // 3. URLify: Write a method to replace all spaces in a string with '%20: You may assume that the string has
  // sufficient space at the end to hold the additional characters, and that you are given the "true" length of the string.
  // (Note: If implementing in Java, please use a character array so that you can perform this operation in place.)
  // EXAMPLE)
  // Input: "Mr John Smith     ", 13
  // Output: "Mr%20John%20Smith"
  def urlify(str: String): String = {
    str.trim.replaceAll(" ", "%20")
  }

  // 4. Palindrome Permutation: Given a string, write a function to check if it is a permutation of a palindrome.
  // A palindrome is a word or phrase that is the same forwards and backwards. A permutation is a rearrangement of letters.
  // The palindrome does not need to be limited to just dictionary words.
  // EXAMPLE)
  // Input: Tact Coa
  // Output: True (permutations: "taco cat", "atco cta", etc.)
  def palindromePermutation(str: String): Boolean = {
    val lowerstr = str.toLowerCase()
    for (i <- 0 to lowerstr.size/2) {
      if (lowerstr(i) != lowerstr(lowerstr.size-1 - i)) return false
    }
    true
  }

  // 5. One Away: There are three types of edits that can be performed on strings: insert a character, remove a character,
  // or replace a character. Given two strings, write a function to check if they are one edit (or zero edits) away.
  // EXAMPLE)
  // pale, ple -> true
  // pales. pale -> true
  // pale. bale -> true
  // pale. bake -> false
  def oneAway(str1: String, str2: String): Boolean = {
    if (Math.abs(str1.size - str2.size) > 1) return false
    str2.map(i => str1.contains(i)).count(_ == false) < 2
  }

  // 6. String Compression: Implement a method to perform basic string compression using the counts of repeated characters.
  // For example, the string aabcccccaaa would become a2b1c5a3. If the "compressed" string would not become smaller than
  // the original string, your method should return the original string. You can assume the string has only uppercase and lowercase letters (a - z).
  def stringCompression(str: String): String = {
    require(str.nonEmpty)
    var outString = ""
    var prevVal = ' '
    var cnt = 1
    prevVal = str(0)
    for (i <- str.substring(1)) {
      if (prevVal == i) cnt += 1
      else {
        outString += prevVal + cnt.toString
        prevVal = i
        cnt = 1
      }
    }
    //insert last value
    outString += prevVal + cnt.toString
    outString
  }

  // 7. Rotate Matrix: Given an image represented by an NxN matrix, where each pixel in the image is 4 bytes, write a method
  // to rotate the image by 90 degrees. (an you do this in place?
  def rotateMatrix90(mat: Array[Array[Int]]): Array[Array[Int]] = {
    val row = mat.size
    val col = mat(0).size
    val newMat = Array.ofDim[Int](col,row)

    for (i <- 0 until row) {
      for (j <- 0 until col) {
        newMat(j)(row-1-i) = mat(i)(j)
      }
    }
    newMat
  }


  // 8. Zero Matrix: Write an algorithm such that if an element in an MxN matrix is 0, its entire row and column are set to O.
  def zeroMatrix(mat: Array[Array[Int]]): Unit ={
    for (i <- mat) {
      for (j <- i.indices) {
        i(j) = 0
      }
    }
  }


  // 9. String Rotation: Assume you have a method isSubstring which checks if one word is a substring of another. Given two strings,
  // 51 and 52, write code to check if 52 is a rotation of 51 using only one call to isSubstring (e.g.,"waterbottle"is a rotation of"erbottlewat").

  def isStringRotation(str1: String, str2: String): Boolean ={
    require(str1.nonEmpty)
    if(str1.size != str2.size) return false

    var str2index = 0
    for (i <- str1.indexOf(str2(str2index)) until str1.size) {
      if (str1(i) != str2(str2index)) return false

      str2index += 1
    }
    for (i <- 0 until str1.indexOf(str2(0))) {
      if (str1(i) != str2(str2index)) return false
      str2index += 1
    }
    true
  }
}
