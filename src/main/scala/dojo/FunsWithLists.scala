package dojo

import collection.immutable.List

object FunsWithLists {

  def labels(ls: List[Game]) = ls map (_.label)

  def averageRatingsOf(l:String,  ls:List[Game]) = {
    /* 1 BONUS POINT : complete this using only higher order functions */
    val criteria: (Game => Boolean) = (_.label == l)
    val gameCount = ls count criteria
    val ratingsTotal = (ls filter criteria).foldLeft(0) (_ + _.rating)
    List(ratingsTotal,gameCount) reduce (_/_)
  }

  def totalRatingsOf(ls: List[Game]):Int = ls match {
    /* 1 BONUS POINT : complete this using recursion */
    case Nil     => 0
    case g :: gs => g.rating + totalRatingsOf(gs)
  }

  def totalRatingsOfLabel(label: String, list: List[Game]):Int = {
    /* 1 BONUS POINT : complete this in an imperative style */
    var total = 0
    for (game <- list) {
      if (game.label == label) {
        total += game.rating
      }
    }
    return total
  }

  def increaseRatingBy(inc: Int, ls: List[Game]) = ls map {g => Game(g.label, g.rating + inc)}

  def decreaseRatingBy(i: Int, s: String, list: List[Game]) = list map {
    g => Game(
      g.label,
      if (g.label == s) g.rating - i else g.rating
    )
  }

  def createFunctionToFindGamesByLabel(label: String):(List[Game]) => List[Game] = _.filter(_.label == label)

  def zipWithKey = (f: (Game) => String, ls: List[Game]) => (ls map f) zip ls

  def firstGameWithRating(r: Int) = (ls: List[Game]) => ls.filter(_.rating == r).head
  //
  // 1. This is almost but not quite equivalent to:
  //
  //        def firstGameWithRating(r: Int)(ls: List[Game]) = ...
  //
  //    which however does not pass the test unless the call to firstGameWithRating is
  //    modified to use partial application syntax (a trailing underscore).
  //
  // 2. The use of filter in this situation is idiomatic in a non-strict language (Haskell),
  //    but in Scala a better way is probably the 'find' method.
}
