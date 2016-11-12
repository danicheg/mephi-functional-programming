package com.github.danicheg

import com.stackmob.newman.ApacheHttpClient
import com.stackmob.newman.dsl._
import com.stackmob.newman.response.HttpResponse

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Try

package object mephiFP {
    def sendSource(filePath: String) ={
        val source = Source.fromFile(filePath)
        val content = Try(source.mkString)

        println(s"${content.getOrElse("false")}")

        implicit val httpClient = new ApacheHttpClient
        val email = "e.danicheg@yandex.ru"

        val response: HttpResponse = Await.result(POST(url(http, "91.239.142.110", 13666) / "lab1")
            .setHeaders("Content-Type" -> "application/x-www-form-urlencoded")
            .setBody(s"email=$email&content=${content.getOrElse("false")}")
            .apply, 2.seconds)
        println(response.bodyString)
    }
}
