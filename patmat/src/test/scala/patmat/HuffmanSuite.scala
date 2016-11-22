package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Fork(
      Leaf('a',8),
      Fork( Fork( Leaf('b',3),
                  Fork( Leaf('c',1),
                        Leaf('d',1),
                        List('c','d'),2),
                  List('b','c','d'),5),
            Fork( Fork( Leaf('e',1),
                        Leaf('f',1),
                        List('e','f'),2),
                  Fork( Leaf('g',1),
                        Leaf('h',1),
                        List('g','h'),2),
                  List('e','f','g','h'),4),
            List('b','d','d','e','f','g','h'), 9),
      List('a','b','d','d','e','f','g','h'), 17)

  }

  test("making a code tree") {
    new TestTrees {
      val sampleTree = makeCodeTree(
        makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
        Leaf('t', 2)
      )
      assert(weight(sampleTree) === 4)
    }
  }

  test("testing pairs") {
    assert(times('e' :: 'x' :: 't' :: Nil)(0) === ('x',1))
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until for some frequency table") {
    until(singleton,combine)(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
  }

  test("encode b with t1") {
    new TestTrees {
      assert(encode(t1)('b' :: Nil) === (1 :: Nil))
    }
  }

  test("encode a with t1") {
    new TestTrees {
      assert(encode(t1)('a' :: Nil) === (0 :: Nil))
    }
  }

  test("encode ab with t1") {
    new TestTrees {
      assert(encode(t1)('a' :: 'b' :: Nil) === (0 :: 1 :: Nil))
    }
  }

  test("decode b with t1") {
    new TestTrees {
      assert(decode(t1, 1 :: Nil) === "b".toList)
    }
  }

  test("decode ab with t1") {
    new TestTrees {
      assert(decode(t1, 0 :: 1 :: Nil) === "ab".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quickEncode should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("covert a tree to codetable and check if encoding matches character") {
    new TestTrees {
      assert(codeBits(convert(t3))('c') === List(1,0,1,0))
    }
  }

  test("do a quickencode of BAC") {
    new TestTrees {
      assert(quickEncode(t3)(List('b','a','c')) === List(1,0,0,0,1,0,1,0))
    }
  }

  test("decode french secret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }
}
