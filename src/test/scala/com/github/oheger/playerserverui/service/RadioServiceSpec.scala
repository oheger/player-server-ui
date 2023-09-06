/*
 * Copyright 2023 Oliver Heger.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.oheger.playerserverui.service

import com.github.oheger.playerserverui.model.RadioModel
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client3.{FetchBackend, HttpError, Request, Response, SttpClientException}
import sttp.model.{Method, StatusCode}

import java.io.IOException
import scala.concurrent.duration.*
import scala.concurrent.{Await, Awaitable, ExecutionContext}

object RadioServiceSpec:
  /** The timeout when waiting for futures to complete. */
  private val AwaitTimeout = 3.seconds

  /** The scheme of the server URI. */
  private val Scheme = "http"

  /** The host of the server URI. */
  private val ServerHost = "192.168.171.12"

  /** The port of the server URI. */
  private val ServerPort = 8088

  /** The base URL for the player service. */
  private val BaseUrl = s"$Scheme://$ServerHost:$ServerPort"

  /**
   * Helper function to check the URI of a request. This function tests whether
   * the URL points to the correct server and the API endpoint. A test function
   * can then test the additional path segments.
   *
   * @param request the request whose URI is to be checked
   * @param f       the test function for the path segments (without the leading
   *                "api" path)
   * @return a flag whether the test is successful
   */
  private def checkUri(request: Request[?, ?])(f: Seq[String] => Boolean): Boolean =
    val uri = request.uri
    uri.scheme.contains(Scheme) && uri.host.contains(ServerHost) && uri.port.contains(ServerPort) &&
      uri.path.startsWith(List("api", "radio")) && f(uri.path.drop(2))

  /**
   * Waits for the given object to complete and returns its value or throws an
   * exception in case of a timeout.
   *
   * @param w the object to wait for
   * @tparam T the type of the result
   * @return the result
   */
  private def await[T](w: Awaitable[T]): T = Await.result(w, AwaitTimeout)
end RadioServiceSpec

/**
 * Test class for [[RadioService]].
 */
class RadioServiceSpec extends AsyncFlatSpec with Matchers:

  import RadioServiceSpec.*

  /**
   * The execution context for asynchronous checks of ScalaTest. This must be
   * the context used by Scala.js.
   */
  implicit override def executionContext: ExecutionContext =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  "RadioService" should "return the available radio sources" in {
    val sourcesJson =
      """
        |{
        |  "sources": [
        |    {
        |      "id": "id1",
        |      "name": "testSource",
        |      "ranking": 10
        |    },
        |    {
        |      "id": "id2",
        |      "name": "anotherSource",
        |      "ranking": 0
        |    }
        |  ]
        |}
        |""".stripMargin
    val expectedSources = List(
      RadioModel.RadioSource("id1", "testSource", 10),
      RadioModel.RadioSource("id2", "anotherSource", 0),
    )

    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("sources")) && request.method == Method.GET
      }.thenRespond(sourcesJson)

    val service = new RadioService(testBackend, BaseUrl)
    service.loadRadioSources() map { sources =>
      sources.sources should contain theSameElementsAs expectedSources
    }
  }

  it should "handle a failure status when querying radio sources" in {
    val testBackend = FetchBackend.stub
      .whenRequestMatches { _ => true }
      .thenRespond(Response("", StatusCode.BadRequest))

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[HttpError[_]] {
      service.loadRadioSources()
    }

    futEx map { ex => ex.getMessage should include(StatusCode.BadRequest.code.toString) }
  }

  it should "handle a failed future when querying radio sources" in {
    val exception = new IOException("test exception")
    val testBackend = FetchBackend.stub
      .whenRequestMatches { _ => true }
      .thenRespond(throw exception)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[SttpClientException] {
      service.loadRadioSources()
    }

    futEx map { ex => ex.cause should be(exception) }
  }

  it should "return information about the current source" in {
    val currentSource =
      """
        |{
        |  "id": "id1",
        |  "name": "currentSource",
        |  "ranking": 10
        |}
        |""".stripMargin
    val playbackState =
      """
        |{
        |  "enabled": false
        |}
        |""".stripMargin
    val expCurrentSource = RadioModel.RadioSource("id1", "currentSource", 10)
    val expCurrentState = RadioService.CurrentSourceState(Some(expCurrentSource), playbackEnabled = false)

    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("sources", "current")) && request.method == Method.GET
      }.thenRespond(currentSource)
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback")) && request.method == Method.GET
      }.thenRespond(playbackState)

    val service = new RadioService(testBackend, BaseUrl)
    service.loadCurrentSource() map { currentStateResult =>
      currentStateResult should be(expCurrentState)
    }
  }

  it should "handle an undefined current source" in {
    val playbackState =
      """
        |{
        |  "enabled": true
        |}
        |""".stripMargin
    val expCurrentState = RadioService.CurrentSourceState(None, playbackEnabled = true)

    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("sources", "current")) && request.method == Method.GET
      }.thenRespond("", StatusCode.NoContent)
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback")) && request.method == Method.GET
      }.thenRespond(playbackState)

    val service = new RadioService(testBackend, BaseUrl)
    service.loadCurrentSource() map { currentStateResult =>
      currentStateResult should be(expCurrentState)
    }
  }

  it should "handle a failure status when querying the current source status" in {
    val playbackState =
      """
        |{
        |  "enabled": true
        |}
        |""".stripMargin
    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback")) && request.method == Method.GET
      }.thenRespond(playbackState)
      .whenRequestMatches { _ => true }
      .thenRespond(Response("", StatusCode.BadRequest))

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[HttpError[_]] {
      service.loadCurrentSource()
    }

    futEx map { ex => ex.getMessage should include(StatusCode.BadRequest.code.toString) }
  }

  it should "handle a failed future when querying the current source status" in {
    val playbackState =
      """
        |{
        |  "enabled": true
        |}
        |""".stripMargin
    val exception = new IOException("test exception")
    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback")) && request.method == Method.GET
      }.thenRespond(playbackState)
      .whenRequestMatches { _ => true }
      .thenRespond(throw exception)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[SttpClientException] {
      service.loadCurrentSource()
    }

    futEx map { ex => ex.cause should be(exception) }
  }

  it should "handle invalid JSON for the current source" in {
    val playbackState =
      """
        |{
        |  "enabled": true
        |}
        |""".stripMargin

    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("sources", "current")) && request.method == Method.GET
      }.thenRespond("Invalid JSON")
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback")) && request.method == Method.GET
      }.thenRespond(playbackState)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[IllegalStateException] {
      service.loadCurrentSource()
    }

    futEx map { ex => ex.getMessage should include("JSON decoding failed") }
  }

  it should "start radio playback" in {
    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback", "start")) && request.method == Method.POST
      }.thenRespond("", StatusCode.Ok)

    val service = new RadioService(testBackend, BaseUrl)

    service.startPlayback().map(_ should be(()))
  }

  it should "handle an unexpected status code when starting radio playback" in {
    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback", "start")) && request.method == Method.POST
      }.thenRespond("", StatusCode.InternalServerError)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[HttpError[_]] {
      service.startPlayback()
    }

    futEx map { ex => ex.getMessage should include(StatusCode.InternalServerError.code.toString) }
  }

  it should "stop radio playback" in {
    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback", "stop")) && request.method == Method.POST
      }.thenRespond("", StatusCode.Ok)

    val service = new RadioService(testBackend, BaseUrl)

    service.stopPlayback().map(_ should be(()))
  }

  it should "handle an exception when stopping radio playback" in {
    val exception = new IOException("Test exception when stopping playback.")
    val testBackend = FetchBackend.stub
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback", "stop")) && request.method == Method.POST
      }.thenRespond(throw exception)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[SttpClientException] {
      service.stopPlayback()
    }

    futEx map { ex => ex.getCause should be(exception) }
  }
