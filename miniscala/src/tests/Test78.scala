package tests

import miniscala.Week7._

object Test78 {

  def main(args: Array[String]): Unit = {
    var mySet: Set[String] = makeEmpty()
    // Test empty set
    assert(size(mySet) == 0)
    assert(isEmpty(mySet))
    assert(!contains(mySet, "abc"))
    // Test add to set
    mySet = add(mySet, "abc")
    assert(contains(mySet, "abc"))
    assert(size(mySet) == 1)
    assert(!isEmpty(mySet))
    // Test repeated add to set
    mySet = add(mySet, "abc")
    assert(contains(mySet, "abc"))
    assert(size(mySet) == 1)
    assert(!isEmpty(mySet))
    // Test add another item to set
    mySet = add(mySet, "def")
    assert(contains(mySet, "abc"))
    assert(contains(mySet, "def"))
    assert(size(mySet) == 2)
    assert(!isEmpty(mySet))
    // Remove from set
    mySet = remove(mySet, "abc")
    assert(!contains(mySet, "abc"))
    assert(size(mySet) == 1)
    // Test union of sets
    var mySet1: Set[String] = makeEmpty()
    mySet1 = add(mySet1, "abc")
    mySet1 = add(mySet1, "def")
    var mySet2: Set[String] = makeEmpty()
    mySet2 = add(mySet2, "abc")
    mySet2 = add(mySet2, "ghi")
    val myUnion = union(mySet1, mySet2)
    assert(size(myUnion) == 3)
    assert(contains(myUnion, "abc"))
    assert(contains(myUnion, "def"))
    assert(contains(myUnion, "ghi"))
    // Set intersection
    val myIntersection = intersection(mySet1, mySet2)
    assert(size(myIntersection) == 1)
    assert(contains(myIntersection, "abc"))
    // Set difference
    val myDifference = difference(mySet1, mySet2)
    assert(size(myDifference) == 1)
    assert(contains(myDifference, "def"))
    val myDifference2 = difference(mySet2, mySet1)
    assert(size(myDifference2) == 1)
    assert(contains(myDifference2, "ghi"))

    println("All tests passed!")
  }

}
