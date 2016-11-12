package com.github.danicheg.mephiFP

object PrettyNumbers extends App {

    /** Get the stream of integers starting from 1
      *
      * @param n number of elements in our stream
      * @return stream of integers
      */
    def getNaturalNumbers(n: Int): Stream[Int] = {
        Stream.from(1).take(n)
    }

    /** Get the list of triangular numbers
      *
      * @param n number of elements that we want to get in resulting list
      * @return list of integers that are triangular numbers
      */
    def getTriangularNumbers(n: Int): List[Int] =
        if (n == 0) List.empty
        else getNaturalNumbers(n).scanLeft(0)(_ + _).toList

    /** Get the list of pyramidal numbers
      *
      * @param n number of elements that we want to get in resulting list
      * @return list of integers that are pyramidal numbers
      */
    def getPyramidalNumbers(n: Int): List[Int] =
        if (n == 0) List.empty
        else getNaturalNumbers(n).scanLeft(0) {
            (x: Int, y: Int) => x + math.pow(y, 2).toInt
        }.toList

}
