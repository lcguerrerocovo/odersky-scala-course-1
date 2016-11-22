package objsets

import TweetReader._

/**
 * A class to represent tweets.
 */

class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

  def <(other: Tweet, ordering: String): Boolean = {
    if(ordering == "retweets")
      this.retweets < other.retweets
    else this.text < other.text
  }

  def >(other: Tweet, ordering: String): Boolean = {
    if(ordering == "retweets")
      this.retweets > other.retweets
    else this.text > other.text
  }
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  val ordering = "text"

  val left : TweetSet

  val right : TweetSet

  val elem : Tweet

  def isEmpty : Boolean

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p,new Empty(this.ordering))
  }
  
  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet
  
  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet
  
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList
  
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet, ordering: String = this.ordering): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  def removeChildrenIfEqualRetweets : TweetSet

  def highestRetweet : Int

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

}

class Empty(override val ordering: String = "text") extends TweetSet {

  val left = null

  val right = null

  val elem = null

  def isEmpty = true

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  override def mostRetweeted: Tweet = throw new NoSuchElementException("set is empty")

  override def descendingByRetweet: TweetList = Nil

  def removeChildrenIfEqualRetweets : TweetSet = this

  override def toString = "."

  def highestRetweet : Int = 0

  /**
    * The following methods are already implemented
    */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet, ordering: String = this.ordering): TweetSet = new NonEmpty(tweet, new Empty(ordering), new Empty(ordering), ordering)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def union(that: TweetSet): TweetSet = that
}

class NonEmpty(val elem: Tweet,val left: TweetSet,val right: TweetSet, override val ordering: String = "text") extends TweetSet {

  def isEmpty = false

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if(p(elem))
      right.filterAcc(p,left.filterAcc(p,acc.incl(elem)))
    else
      right.filterAcc(p,left.filterAcc(p,acc))
  }

  def union(that: TweetSet): TweetSet = {
    // TODO optimize?
    //((left union right) union that) incl elem
    //println(united)
    //united

    def iterUnion(acc: TweetSet, that: TweetList) : TweetSet = {
      if(that.isEmpty) acc
      else iterUnion(acc incl that.head, that.tail)
    }
    iterUnion(this,that.descendingByRetweet)
  }

  override def mostRetweeted: Tweet = {
    //require(this.ordering == "retweets","TweetSet must be ordered by retweets")
    if(this.ordering == "retweets") {
      def iter(set: TweetSet): Tweet = {
        if(set.right.isEmpty) set.elem
        else iter(set.right)
      }
      iter(this)
    } else {
      def iter(set: TweetSet): TweetSet = {
        if (set.right.isEmpty && set.left.isEmpty) set
        else {
          val highest = set.highestRetweet
          iter(filter((tweet: Tweet) => highest <= tweet.retweets).removeChildrenIfEqualRetweets)
        }
      }
      iter(this).elem
    }
  }

  def highestRetweet : Int = {
    val leftRt = if(!left.isEmpty) left.elem.retweets else 0
    val rightRt = if(!right.isEmpty) right.elem.retweets else 0
    //println("elem.retweets:" + elem.retweets)
    //println("leftRt:" + leftRt)
    //println("rightRt:" + rightRt)

    if(elem.retweets >= leftRt && elem.retweets >= rightRt) elem.retweets
    else if(elem.retweets >= leftRt) rightRt
    else leftRt
  }

  def removeChildrenIfEqualRetweets : TweetSet = {
    if(!left.isEmpty && left.elem.retweets == elem.retweets)
      this.remove(left.elem).removeChildrenIfEqualRetweets
    else if(!right.isEmpty && right.elem.retweets == elem.retweets)
      this.remove(right.elem)
    else this
  }

  def descendingByRetweet: TweetList = {
    def iter(set: TweetSet) : TweetList = {
      if(set.isEmpty) Nil
      else {
        val mostRetweetedTweet = set.mostRetweeted
        new Cons(mostRetweetedTweet,iter(set.remove(mostRetweetedTweet)))
      }
    }
    iter(this)
  }

  override def toString = "{" + left.toString + elem.retweets + right.toString + "}"

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet, ordering: String = this.ordering): TweetSet = {
    if (x.<(elem,ordering)) new NonEmpty(elem, left.incl(x), right,ordering)
    else if (elem.<(x,ordering)) new NonEmpty(elem, left, right.incl(x),ordering)
    else this
  }

  def remove(tw: Tweet): TweetSet = {
    if (tw.<(elem,this.ordering)) new NonEmpty(elem, left.remove(tw), right, this.ordering)
    else if (elem.<(tw,this.ordering)) new NonEmpty(elem, left, right.remove(tw), this.ordering)
    else left.union(right)
  }

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
  override def toString = "[ Nil ]"
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
  override def toString = "[" + head.retweets + " , " + tail.toString + "]"
}


object GoogleVsApple {

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
  val tweets = TweetReader.tweetSets

  lazy val googleTweets: TweetSet = getTweets(google)

  lazy val appleTweets: TweetSet = getTweets(apple)

  lazy val googleAppleTweets: TweetSet = getTweets(List.concat(google,apple))


  def getTweets(companyList: List[String]) : TweetSet = {
    def iter(setList: List[TweetSet], list: List[String]) : TweetSet = {
      if(setList.isEmpty) new Empty("retweets")
      else if(list.isEmpty) iter(setList.tail,companyList)
      else setList.head.filter((tweet: Tweet) => tweet.text.contains(list.head)) union iter(setList,list.tail)
    }
    iter(tweets,companyList)
  }
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
     lazy val trending: TweetList = (googleAppleTweets).descendingByRetweet
  }

object Main extends App {
  println(GoogleVsApple.googleAppleTweets)
  println(GoogleVsApple.trending)
}
