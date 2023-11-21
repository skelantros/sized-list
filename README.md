This repository contains a simple implementation of a compile-time sized linked list `SList[A, N <: Int]`, where `N` is a literal type of its size.

The list is implemented using Scala 3 metaprogramming features in macros-less approach.

Some examples `SList`'s capabilities:
```scala
// list creation
val intList: SList[Int, 3] = 1 :: 2 :: 3 :: SNil
val stringList: SList[String, 2] = "foo" :: "bar" :: SNil
val emptyList: SList[Int, 0] = SNil

// list type refinement based on its size
val refinedIntList: SCons[Int, 2] = intList.refined
val refinedEmptyList: SNil.type = emptyList.refined

// compile-time safe head/tail
stringList.head // foo
stringList.tail // SList(bar): SList[String, 1]

// compile time error: the size of stringList.tail.tail is 0
stringList.tail.tail.head
stringList.tail.tail.tail

// a for-comprehension example

// SList(1foo,1bar,2foo,2bar,3foo,3bar)
val combinedList: SList[String, 6] = for {
	int <- intList
	string <- stringList
	resultValue = int.toString + string
} yield resultValue
```