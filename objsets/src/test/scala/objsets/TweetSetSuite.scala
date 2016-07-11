package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 21)
    val d = new Tweet("d", "d body", 27)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("mostRetweeted: ") {
    new TestSets {
      assert(set2.mostRetweeted.retweets === 20)
      assert(set4c.mostRetweeted.retweets === 21)
      assert(set4d.mostRetweeted.retweets === 27)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "d")
      assert(trends.tail.head.user == "c")
      assert(trends.tail.tail.head.user == "a" || trends.tail.tail.head.user == "b")
    }
  }

  test("List behavior") {
    new TestSets {
      val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
      val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
      val appleText = "These new Apple patents give a sneak peek at what future iPhone cameras might have in store. http://t.co/0YT9rjxp"
      val googleText = "Ooh, a galaxy-shooting camera you might actually afford http://t.co/VLXkarGV"
      val amazonText = "Kindle Paperwhite Review: Forget Everything Else, This Is the E-Reader You Want http://t.co/737W6aNC"

      def hasWord(words: List[String], text: String) : Boolean  =
        if (words.isEmpty) false
        else if (text.contains(words.head)) true
        else hasWord(words.tail, text)

      assert(hasWord(google, googleText))
      assert(hasWord(apple, appleText))
      assert(!hasWord(google, amazonText))
    }
  }

  }
