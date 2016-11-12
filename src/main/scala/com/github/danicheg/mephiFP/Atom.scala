package com.github.danicheg.mephiFP

import scala.annotation.tailrec

object Atom extends App {

    //or we can use List(...).distinct
    /** Implementation of distinct method (with saving order) from Scala library
      *
      * @param args list of atoms
      * @return list of unique atoms
      */
    def set(args: List[Any]) = {

        /** Nested function of set that implement it logic
          *
          * @param inner list that will be returned
          * @param outer list that will be traversed
          * @return list of unique atoms
          */
        @tailrec
        def prepare(inner: List[Any], outer: List[Any]): List[Any] = outer match {
            case Nil => inner
            case x :: xs =>
                if (contains(x, inner)) prepare(inner, xs)
                else prepare(inner ::: List(x), xs)
        }

        /** Implementation of contains method from Scala library
          *
          * @param elem element that will be checked
          * @param list list that will be traversed
          * @return boolean value of search result
          */
        @tailrec
        def contains(elem: Any, list: List[Any]): Boolean = list match {
            case Nil => false
            case x :: xs =>
                if (x == elem) true
                else contains(elem, xs)
        }

        prepare(List.empty, args)
    }

    //or we can use List(...).distinct.zip(List(...).distinct.map(elem => List(...).count(_ == elem)))
    /** Function that returned list of tuples (element list, count of it occurrences)
      *
      * @param args initial list that will be traversed
      * @return list of pairs (element, count of occurrences)
      */
    def freq(args: List[Any]) = {

        /** Function
          *
          * @param elem element that will be checked
          * @param list list that will be traversed
          * @param counter counter
          * @return
          */
        @tailrec
        def count(elem: Any, list: List[Any], counter: Int): Int = list match {
            case Nil => counter
            case x :: xs =>
                if (x == elem) count(elem, xs, counter+1)
                else count(elem, xs, counter)
        }

        /** Function that implement logic of freq() function
          *
          * @param inner list that will be returned
          * @param outer list that will be traversed
          * @return list of tuples (element, count of occurrences)
          */
        @tailrec
        def prepare(inner: List[(Any, Int)], outer: List[Any]): List[(Any, Int)] = outer match {
            case Nil => inner
            case x :: xs => prepare(inner ::: List((x, count(x, args, 0))), xs)
        }

        //use set() function implemented earlier
        prepare(List.empty, set(args))
    }

}
