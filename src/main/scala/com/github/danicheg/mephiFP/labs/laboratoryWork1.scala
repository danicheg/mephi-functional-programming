package com.github.danicheg.mephiFP.labs

import java.nio.file.Paths

import com.github.danicheg.mephiFP._

import scala.annotation.tailrec
import scala.math._

object laboratoryWork1 extends App {

    //parameters for first task
    val func = (x:Double) => x/(9+x*x)
    val nthElementOfTaylorSeries = (x: Double, n: Int) =>
        (pow(-1.0, n.toDouble)*pow(x, (2*n + 1).toDouble)) / pow(9.0, (n+1).toDouble)
    val (lowerBound, upperBound, precision, steps) = (-1.0, 1.0, 1e-5, 20)
    val step = (upperBound - lowerBound) / steps

    TaylorSeries.printTable(func, nthElementOfTaylorSeries, lowerBound, upperBound, precision, step)

    //parameters for second task
    val epsilon = 1e-2
    val equations = List(
        ((x: Double) => exp(x) + log(x) - 10 * x, (3.0, 4.0), 3.5265),
        ((x: Double) => cos(x) - exp(-pow(x, 2) / 2) + x - 1, (1.0, 2.0), 1.0804),
        ((x: Double) => 1 - x + sin(x) - log(1+x), (1.0, 1.5), 1.1474)
    )

    val dFunctions = List(
        (x: Double) => exp(x) + 1 / x - 10,
        (x: Double) => x * exp(-pow(x, 2) / 2) - sin(x) + 1,
        (x: Double) => -1 + cos(x) - (1/(1+x))
    )

    val functionsIterativeMethod = List(
        (x: Double) => log(10 * x - log(x)),
        (x: Double) => 1 + exp(-pow(x, 2) / 2) - cos(x),
        (x: Double) => 1 + sin(x) - log(1+x)
    )

    (equations zip dFunctions zip functionsIterativeMethod).foreach {
        case (((f, bnd, vl), df), fim) =>
            Equations.print(Equations.newtonMethod(f, df, bnd, precision), "Newton Method")
            Equations.print(Equations.dichotomyMethod(f, bnd, epsilon), "Dichotomy Method")
            Equations.print(Equations.iterativeMethod(fim, bnd, precision), "Iterative Method")
    }

    val currentDirectory: String = Paths.get("").toAbsolutePath.toString
    val filePath: String = currentDirectory + "\\src\\main\\scala\\com\\github\\danicheg\\mephiFP\\laboratoryWork1.scala"

    //send source via http client
    sendSource(filePath)
}

object TaylorSeries {

    def getFunctionValues(func: Double => Double, nthElem: (Double, Int) => Double,
                          lb: Double, ub: Double, eps: Double, step: Double) = {

        def getStandardSeries: List[(Double, Double)] =
            (for (i <- Range.Double(lb, ub + step, step)) yield (i, func(i))).toList

        def getTaylorSeries = {

            def isCloseEnough(value: Double, interval: (Double, Double)) = value > interval._1 && value < interval._2
            def getElem(point: Double, n: Int) = (for(i <- 0 to n) yield  nthElem(point, i)).sum
            def getInterval(res: Double) = (res - eps, res + eps)

            @tailrec
            def gen(step: Int, from: Double, bmk: Double): (Int, Double, Double, Double) =
                if (isCloseEnough(getElem(from, step-1), getInterval(bmk))) (step, from, bmk, getElem(from, step-1))
                else gen(step+1, from, bmk)

            def eval(sl: List[(Double, Double)]): List[(Int, Double, Double, Double)] = sl.map(i => gen(1, i._1, i._2))

            eval(getStandardSeries)
        }

        getTaylorSeries
    }

    def printTable(func: Double => Double, nthElem: (Double, Int) => Double,
                   lb: Double, ub: Double, eps: Double, step: Double) = {
        val result = getFunctionValues(func, nthElem, lb, ub, eps, step)

        println("[TABLE OF VALUES OF FUNCTION]")
        println("number of steps \t\t point \t\t value of standard method \t\t value of Taylor method")
        result foreach {
            case (stp, pnt, vsm, stm) => println(s"$stp \t\t\t\t\t\t $pnt \t\t $vsm \t\t\t $stm")
        }
    }

}

object Equations {

    type Result = (Int, Double)

    def newtonMethod(f: Double => Double, df: Double => Double, bound: (Double, Double), eps: Double) = {

        def getNextValue(x: Double) = x - f(x)/df(x)
        def isCloseEnough(x: Double) = abs(getNextValue(x) - x) < eps

        @tailrec
        def eval(x: Double, n: Int): Result = {
            if (isCloseEnough(x)) (n, getNextValue(x))
            else eval(getNextValue(x), n+1)
        }

        eval((bound._1 + bound._2)/2, 1)
    }

    def dichotomyMethod(f: Double => Double, bound: (Double, Double), eps: Double) = {

        @tailrec
        def eval(brd: (Double, Double), step: Int = 1): Result = {
            val middle = (brd._1 + brd._2)/2
            def isCloseEnough = abs(brd._2 - brd._1) < eps
            def isNegative = f(brd._1) * f (middle) < 0

            if (isCloseEnough) (step, middle)
            else if (isNegative) eval((brd._1, middle), step+1)
            else eval((middle, brd._2), step+1)
        }

        eval(bound)
    }

    def iterativeMethod(f: Double => Double, bound: (Double, Double), eps: Double) = {

        def isCloseEnough(x: Double) = abs(f(x) - x) < eps

        @tailrec
        def eval(x: Double, n: Int): Result = {
            if (isCloseEnough(x)) (n, f(x))
            else eval(f(x), n+1)
        }

        eval((bound._1 + bound._2)/2, 1)
    }

    def print(result: Result, methodName: String) = {
        println(s"[Solving equation using $methodName]")
        println("number of step \t\t result")
        result match {
            case (stp, res) => println(s"$stp \t\t\t\t $res \n")
        }
    }

}
