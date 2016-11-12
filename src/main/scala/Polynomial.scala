object Polynomial extends App {

    val func = (x:Double) => x/(9+x*x)
    val nthElementOfTaylorSeries = (x: Double, n: Int) =>
        (math.pow(-1.0, n.toDouble)*math.pow(x, (2*n + 1).toDouble)) / math.pow(9.0, (n+1).toDouble)
    val (lowerBound, upperBound, precision, steps) = (-1.0, 1.0, 1e-4, 20)
    val step = (upperBound - lowerBound) / steps

    def getTableOfValues(func: Double => Double, nthElem: (Double, Int) => Double,
                         lb: Double, ub: Double, prcsn: Double, step: Double) = {

        def getStandardSeries: List[(Double, Double)] =
            (for( i <- Range.Double(lb, ub + step, step)) yield (i, func(i))).toList

        def getTaylorSeries = {

            def isCloseEnough(value: Double, interval: (Double, Double)) = value > interval._1 && value < interval._2
            def getElem(point: Double, n: Int) = (for(i <- 0 to n) yield  nthElem(point, i)).sum
            def getInterval(res: Double) = (res - prcsn, res + prcsn)

            def gen(step: Int, from: Double, bmk: Double): (Int, Double, Double, Double) =
                if (isCloseEnough(getElem(from, step), getInterval(bmk))) (step+1, from, bmk, getElem(from, step))
                else gen(step+1, from, bmk)

            def genTable(sl: List[(Double, Double)]) = sl.map(i => gen(0, i._1, i._2))

            genTable(getStandardSeries)
        }

        getTaylorSeries
    }

    getTableOfValues(func, nthElementOfTaylorSeries, lowerBound, upperBound, precision, step).foreach(println)
}