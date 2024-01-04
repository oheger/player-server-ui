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
import com.github.oheger.playerserverui.model.RadioModel.RadioMessage
import com.raquo.airstream.core.{EventStream, Observer}
import com.raquo.airstream.ownership.ManualOwner
import org.scalatest.BeforeAndAfterAll
import org.scalatest.compatible.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.capabilities.WebSockets
import sttp.client3.testing.SttpBackendStub
import sttp.client3.{HttpError, Request, Response, SttpClientException}
import sttp.model.{Method, StatusCode}
import sttp.ws.WebSocketFrame
import sttp.ws.testing.WebSocketStub
import zio.json.*

import java.io.IOException
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.*

object RadioServiceSpec:
  /** The scheme of the server URI. */
  private val Scheme = "http"

  /** The host of the server URI. */
  private val ServerHost = "192.168.171.12"

  /** The port of the server URI. */
  private val ServerPort = 8088

  /** The base URL for the player service. */
  private val BaseUrl = s"$Scheme://$ServerHost:$ServerPort"

  /** The path prefix for invoking the radio API. */
  private val RadioApiPrefix = List("api", "radio")

  /** The default timeout when waiting for a test to succeed. */
  private val DefaultTimeout = 3.seconds

  /** An encoder to convert radio messages to JSON. */
  private implicit val radioMessageEncoder: JsonEncoder[RadioModel.RadioMessage] =
    DeriveJsonEncoder.gen[RadioModel.RadioMessage]

  /**
   * Creates the backend to be used for tests.
   *
   * @return the test backend
   */
  private def createTestBackend(): SttpBackendStub[Future, WebSockets] =
    SttpBackendStub.asynchronousFuture

  /**
   * Helper function to check the URI of a request. This function tests whether
   * the URL points to the correct server and the API endpoint. A test function
   * can then test the additional path segments.
   *
   * @param request the request whose URI is to be checked
   * @param prefix  the path prefix for the request
   * @param scheme  the expected URI scheme
   * @param f       the test function for the path segments (without the leading
   *                "api" path)
   * @return a flag whether the test is successful
   */
  private def checkUri(request: Request[?, ?],
                       prefix: List[String] = RadioApiPrefix,
                       scheme: String = Scheme)
                      (f: Seq[String] => Boolean): Boolean =
    val uri = request.uri
    uri.scheme.contains(scheme) && uri.host.contains(ServerHost) && uri.port.contains(ServerPort) &&
      uri.path.startsWith(prefix) && f(uri.path.drop(prefix.size))

  /**
   * Returns a [[Promise]] that can be used for an asynchronous test. The
   * promise is configured to fail after a given timeout.
   *
   * @param timeout the timeout for the promise
   * @param owner   the owner for observer registrations
   * @tparam T the result type of the promise
   * @return the promise with a timeout
   */
  private def promiseWithTimeout[T](timeout: Duration = DefaultTimeout)(implicit owner: ManualOwner): Promise[T] =
    val promise = Promise[T]()
    val timeoutStream = EventStream.delay(DefaultTimeout.toMillis.toInt, (), emitOnce = true)
    val observer = Observer[Unit] { _ =>
      promise.tryFailure(new IllegalStateException("Promise did not complete within timeout."))
    }
    timeoutStream.addObserver(observer)
    promise

end RadioServiceSpec

/**
 * Test class for [[RadioService]].
 */
class RadioServiceSpec extends AsyncFlatSpec with BeforeAndAfterAll with Matchers:

  import RadioServiceSpec.*

  /**
   * The execution context for asynchronous checks of ScalaTest. This must be
   * the context used by Scala.js.
   */
  implicit override def executionContext: ExecutionContext =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  /** An owner for airstream subscriptions. */
  implicit val owner: ManualOwner = new ManualOwner

  override protected def afterAll(): Unit =
    owner.killSubscriptions()
    super.afterAll()

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

    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("sources")) && request.method == Method.GET
      }.thenRespond(sourcesJson)

    val service = new RadioService(testBackend, BaseUrl)
    service.loadRadioSources() map { sources =>
      sources.sources should contain theSameElementsAs expectedSources
    }
  }

  it should "handle a failure status when querying radio sources" in {
    val testBackend = createTestBackend()
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
    val testBackend = createTestBackend()
      .whenRequestMatches { _ => true }
      .thenRespond(throw exception)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[SttpClientException] {
      service.loadRadioSources()
    }

    futEx map { ex => ex.cause should be(exception) }
  }

  it should "return information about the current source state" in {
    val currentSourceState =
      """
        |{
        |  "currentSourceId": "id1",
        |  "replacementSourceId": "id2",
        |  "titleInfo": "some title"
        |}
        |""".stripMargin
    val playbackState =
      """
        |{
        |  "enabled": false
        |}
        |""".stripMargin
    val expCurrentState = RadioService.CurrentSourceState(Some("id1"),
      Some("id2"),
      playbackEnabled = false,
      Some("some title"))

    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("sources", "current")) && request.method == Method.GET &&
          request.uri.params.get("full").contains("true")
      }.thenRespond(currentSourceState)
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback")) && request.method == Method.GET
      }.thenRespond(playbackState)

    val service = new RadioService(testBackend, BaseUrl)
    service.loadCurrentSource() map { currentStateResult =>
      currentStateResult should be(expCurrentState)
    }
  }

  it should "handle an undefined current source and replacement source" in {
    val playbackState =
      """
        |{
        |  "enabled": true
        |}
        |""".stripMargin
    val expCurrentState = RadioService.CurrentSourceState(None, None, playbackEnabled = true, None)

    val testBackend = createTestBackend()
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
    val testBackend = createTestBackend()
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
    val testBackend = createTestBackend()
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

    val testBackend = createTestBackend()
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
    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback", "start")) && request.method == Method.POST
      }.thenRespond("", StatusCode.Ok)

    val service = new RadioService(testBackend, BaseUrl)

    service.startPlayback().map(_ should be(()))
  }

  it should "handle an unexpected status code when starting radio playback" in {
    val testBackend = createTestBackend()
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
    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback", "stop")) && request.method == Method.POST
      }.thenRespond("", StatusCode.Ok)

    val service = new RadioService(testBackend, BaseUrl)

    service.stopPlayback().map(_ should be(()))
  }

  it should "handle an exception when stopping radio playback" in {
    val exception = new IOException("Test exception when stopping playback.")
    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("playback", "stop")) && request.method == Method.POST
      }.thenRespond(throw exception)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[SttpClientException] {
      service.stopPlayback()
    }

    futEx map { ex => ex.getCause should be(exception) }
  }

  it should "switch to another radio source" in {
    val RadioSourceID = "nextCurrentRadioSource"
    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request)(path => path.toList == List("sources", "current", RadioSourceID)) &&
          request.method == Method.POST
      }.thenRespond("", StatusCode.Ok)

    val service = new RadioService(testBackend, BaseUrl)

    service.changeCurrentSource(RadioSourceID).map(_ should be(()))
  }

  it should "handle an unexpected status code when switching to another source" in {
    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request)(_.toList.take(2) == List("sources", "current")) && request.method == Method.POST
      }.thenRespond("", StatusCode.NotFound)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[HttpError[_]] {
      service.changeCurrentSource("someSourceID")
    }

    futEx map { ex => ex.getMessage should include(StatusCode.NotFound.code.toString) }
  }

  it should "handle an exception when switching to another source" in {
    val exception = new IOException("Test exception when switching to a radio source.")
    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request)(_.toList.take(2) == List("sources", "current")) && request.method == Method.POST
      }.thenRespond(throw exception)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[SttpClientException] {
      service.changeCurrentSource("failureSource")
    }

    futEx map { ex => ex.getCause should be(exception) }
  }

  it should "support shutting down the server" in {
    val promiseInvocation = Promise[Assertion]()

    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request, List("api"))(_.toList == List("shutdown")) && request.method == Method.POST
      }.thenRespond {
        promiseInvocation.success(true shouldBe true)
        throw new IOException("Server shutting down.")
      }
    val service = new RadioService(testBackend, BaseUrl)
    service.shutdown()

    promiseInvocation.future
  }

  it should "support registering an event listener" in {
    val radioMessages = List(
      RadioModel.RadioMessage(RadioModel.RadioMessageType.SourceChanged.toString, "someSource"),
      RadioModel.RadioMessage(RadioModel.RadioMessageType.TitleInfo.toString, "some title")
    )
    val webSocketStub = WebSocketStub.initialReceive(
      radioMessages.map(msg => WebSocketFrame.text(msg.toJson))
    )
    val testBackend = createTestBackend()
      .whenRequestMatches { request =>
        checkUri(request, scheme = "ws")(_.toList == List("events"))
      }.thenRespond(webSocketStub)

    val service = new RadioService(testBackend, BaseUrl)
    val promiseMessages = promiseWithTimeout[List[RadioModel.RadioMessage]]()
    var messages = List.empty[RadioModel.RadioMessage]
    val listener: RadioModel.RadioMessage => Unit = { msg =>
      messages = msg :: messages
      if messages.size >= 2 then promiseMessages.success(messages)
    }

    service.registerEventListener(listener)

    promiseMessages.future map { messages =>
      messages should contain theSameElementsAs radioMessages
    }
  }

  it should "handle an exception when registering an event listener" in {
    val exception = new IOException("Test exception when registering an event listener.")
    val testBackend = createTestBackend()
      .whenAnyRequest
      .thenRespond("", StatusCode.Forbidden)

    val service = new RadioService(testBackend, BaseUrl)
    val futEx = recoverToExceptionIf[HttpError[_]] {
      service.registerEventListener(println)
    }

    futEx map { ex => ex.getMessage should include(StatusCode.Forbidden.code.toString) }
  }

  it should "ignore web socket messages that cannot be deserialized to radio messages" in {
    val radioMessage = RadioModel.RadioMessage(RadioModel.RadioMessageType.ReplacementEnd.toString, "currentSource")
    val webSocketStub = WebSocketStub.initialReceive(
      List("This is an unexpected message", radioMessage.toJson).map(WebSocketFrame.text)
    )
    val testBackend = createTestBackend()
      .whenAnyRequest
      .thenRespond(webSocketStub)

    val service = new RadioService(testBackend, BaseUrl)
    val promiseMessage = promiseWithTimeout[RadioModel.RadioMessage]()
    val listener: RadioModel.RadioMessage => Unit = { msg =>
      promiseMessage.success(msg)
    }

    service.registerEventListener(listener)

    promiseMessage.future map { message =>
      message should be(radioMessage)
    }
  }
