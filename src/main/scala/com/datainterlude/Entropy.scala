package com.datainterlude

object Entropy {

  def compute(probs: Array[Double], k: Option[Int] = None): Double = {
    val positiveProbs = probs.filter(_ > 0)
    val sum = positiveProbs.sum
    val parcels = positiveProbs map { prob =>
      val normalized = prob / sum
      normalized * math.log(normalized)
    }
    -1.0 * parcels.sum / math.log(k.getOrElse(probs.length).toDouble)
  }

  def compute(prob: Double) = compute(Array(prob, 1-prob), Some(2))

  def expected(ofy: List[Boolean], givenx: List[Double]): (Double, Double) = {
    val n = ofy.length.toDouble
    val cuts = percentiles(givenx)
    val entropies = cuts map { cut =>
      ofy.zipWithIndex
        .groupBy { case (_, i) => givenx(i) > cut }
        .mapValues { featureResponsePairs =>
          val positives = featureResponsePairs count { case (yIsTrue, _) => yIsTrue }
          (featureResponsePairs.length, positives)
        }
        .toList
        .map { case (total, positives) => total/n * compute(positives/total) }
        .sum
    }

    val (bestCut, andItsEntropy) = cuts zip entropies minBy { _._2 }
    (bestCut, andItsEntropy)
  }

  def expected(ofy: List[Boolean], givenx: List[String]) = {
    val n = ofy.length.toDouble
    val xs = givenx groupBy identity mapValues { _.length }
    val nxs = xs.size

    val xsWithYProbs = xs.keysIterator map { x =>
      val ys = ofy.zipWithIndex
        .filter { case (_, i) => givenx(i) == x }
        .map    { case (y, _) => y }
      (ys count identity) / ys.length.toDouble
    } zip xs.keysIterator
    val sortedxs = xsWithYProbs.toList sortBy { _._1 } map { _._2 }

    val entropies = 0 until (nxs-1) map { i =>
      val cut = sortedxs.slice(i+1, nxs)
      ofy
        .zipWithIndex
        .groupBy { case (_, i) => cut contains givenx(i) }
        .mapValues { yxpairs =>
          val positives = yxpairs count { case (yIsTrue, _) => yIsTrue }
          (yxpairs.length, positives.toDouble)
        }
        .toList
        .map { case (_, (total, positives)) => total/n * compute(positives/total) }
        .sum
    }

    val bestCutAt = entropies.zipWithIndex minBy { _._1 } map { _._2 }
    val bestCut = sortedxs.zipWithIndex filter { _._2 > bestCutAt } map { _._1 }
    (bestCut, entropies(bestCutAt))
  }

  def percentiles(xs: List[Double]): List[Double] = {
    val n = xs.length.toDouble
    val probabilities = (1 to 99).map(_.toDouble / 100).toList
    val sorted = xs sortBy identity
    probabilities map { prob => sorted(math.floor(prob * n).toInt) }
  }

  def main(args: Array[String]): Unit = {
    val n = 20
    val xs = List("a", "b", "c")
    val y = (1 to n map {i => math.random > 0.5 }).toList
    val x = (1 to n map {i =>  }).toList
  }
}







