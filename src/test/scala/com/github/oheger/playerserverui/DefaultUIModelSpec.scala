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

package com.github.oheger.playerserverui

import com.github.oheger.playerserverui.model.RadioModel
import com.github.oheger.playerserverui.service.RadioService
import com.raquo.airstream.core.{EventStream, Observer, Signal}
import com.raquo.airstream.ownership.ManualOwner
import com.raquo.airstream.state.{StrictSignal, Var}
import org.scalatest.compatible.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object DefaultUIModelSpec:
  /** Timeout when waiting for a specific signal value. */
  private val SignalTimeoutMs = 3000

  /**
   * The URL to be used when initializing a radio service. Since the service is
   * not actually invoked, this is a dummy URL.
   */
  private val ServiceUrl = "https://radio.example.org"
end DefaultUIModelSpec

/**
 * Test class for [[DefaultUIModel]].
 */
class DefaultUIModelSpec extends AsyncFlatSpec with Matchers:
  /**
   * The execution context for asynchronous checks of ScalaTest. This must be
   * the context used by Scala.js.
   */
  implicit override def executionContext: ExecutionContext =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  import DefaultUIModelSpec.*

  /**
   * Helper function for checking whether a signal takes a specific value. The
   * function checks asynchronously whether a value as specified by a condition
   * is found within a timeout. Otherwise, a failed assertion is returned.
   *
   * @param signal the signal to test
   * @param test   the function to check the signal value
   * @tparam A the type of the signal
   * @return a ''Future'' with an assertion to check asynchronously
   */
  private def assertSignal[A](signal: Signal[A])(test: A => Boolean): Future[Assertion] =
    val promise = Promise[Boolean]()

    // Test the current value first. It will not occur in the changes stream.
    val currentValue = signal.asInstanceOf[StrictSignal[A]].now()
    if test(currentValue) then
      Future {
        true shouldBe true
      }
    else
      implicit val owner: ManualOwner = new ManualOwner
      val signalChanges = signal.changes
        .filter(test)
        .map(_ => true)
      val timeout = EventStream.delay(SignalTimeoutMs, false, emitOnce = true)
      val observer = Observer[Boolean](f => promise.success(f))
      signalChanges.addObserver(observer)
      timeout.addObserver(observer)
      promise.future.map { result => result shouldBe true } andThen { _ => owner.killSubscriptions() }

  /**
   * Helper function for checking a signal against a specific value. This
   * function is analogous to ''assertSignal()'', but instead of a check
   * function, an expected value can be specified directly.
   *
   * @param signal        the signal to test
   * @param expectedValue the expected value of this signal
   * @tparam A the type of the signal
   * @return a ''Future'' with an assertion that checks the signal value
   */
  private def assertSignalValue[A](signal: Signal[A], expectedValue: A): Future[Assertion] =
    assertSignal(signal)(_ == expectedValue)

  "initRadioSources" should "fetch the radio sources from the server" in {
    val RadioSourcesList = List(
      RadioModel.RadioSource("s1", "RadioSource1", 1),
      RadioModel.RadioSource("s2", "RadioSource2", 2),
      RadioModel.RadioSource("s3", "RadioSource3", 3),
    )
    val service = new RadioService(ServiceUrl) {
      override def loadRadioSources(): Future[RadioModel.RadioSources] =
        Future.successful(RadioModel.RadioSources(RadioSourcesList))
    }

    val model = new DefaultUIModel(service)
    model.initRadioSources()

    assertSignalValue(model.radioSourcesSignal, Some(Success(RadioSourcesList)))
  }

  it should "record an exception when loading the radio sources from the server" in {
    val exception = new IllegalStateException("Test exception when loading radio sources.")
    val service = new RadioService(ServiceUrl) {
      override def loadRadioSources(): Future[RadioModel.RadioSources] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.initRadioSources()

    assertSignalValue(model.radioSourcesSignal, Some(Failure(exception)))
  }

  it should "initially have a value of None" in {
    val model = new DefaultUIModel(new RadioService(ServiceUrl))

    assertSignalValue(model.radioSourcesSignal, None)
  }

  "initRadioPlaybackState" should "fetch the current radio source from the server" in {
    val service = new RadioServiceTestImpl

    val model = new DefaultUIModel(service)
    model.initRadioPlaybackState()

    assertSignalValue(model.radioPlaybackStateSignal, Some(Success(DummyUIModel.TestRadioPlaybackState)))
  }

  it should "record an exception when loading the current radio source from the server" in {
    val exception = new IllegalStateException("Test exception when loading the current radio source.")
    val service = new RadioServiceTestImpl {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.initRadioPlaybackState()

    assertSignalValue(model.radioPlaybackStateSignal, Some(Failure(exception)))
  }

  it should "record an exception when registering an event listener" in {
    val exception = new IllegalStateException("Test exception when registering a radio event listener.")
    val service = new RadioService(ServiceUrl) {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.successful(DummyUIModel.CurrentSource)

      override def registerEventListener(listener: RadioModel.RadioMessage => Unit): Future[Unit] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.initRadioPlaybackState()

    assertSignalValue(model.radioPlaybackStateSignal, Some(Failure(exception)))
  }

  it should "initially have a value of None" in {
    val model = new DefaultUIModel(new RadioService(ServiceUrl))

    assertSignalValue(model.radioPlaybackStateSignal, None)
  }

  "startRadioPlayback" should "update the playback state" in {
    val currentSourceState = DummyUIModel.CurrentSource.copy(playbackEnabled = false)
    val initialRadioState = DummyUIModel.TestRadioPlaybackState.copy(playbackEnabled = false)
    val service = new RadioServiceTestImpl {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.successful(currentSourceState)

      override def startPlayback(): Future[Unit] =
        Future.successful(())
    }

    val model = new DefaultUIModel(service)
    model.initRadioPlaybackState()

    assertSignalValue(model.radioPlaybackStateSignal, Some(Success(initialRadioState))) flatMap { _ =>
      model.startRadioPlayback()
      assertSignalValue(model.radioPlaybackStateSignal, Some(Success(DummyUIModel.TestRadioPlaybackState)))
    }
  }

  it should "handle a failed update" in {
    val exception = new IllegalStateException("Test exception when starting playback.")
    val currentSourceState = DummyUIModel.CurrentSource.copy(playbackEnabled = false)
    val service = new RadioServiceTestImpl {
      override def startPlayback(): Future[Unit] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.initRadioPlaybackState()
    model.startRadioPlayback()

    assertSignalValue(model.radioPlaybackStateSignal, Some(Failure(exception)))
  }

  "stopRadioPlayback" should "update the playback state" in {
    val expCurrentSource = DummyUIModel.TestRadioPlaybackState.copy(playbackEnabled = false)
    val service = new RadioServiceTestImpl {
      override def stopPlayback(): Future[Unit] =
        Future.successful(())
    }

    val model = new DefaultUIModel(service)
    model.initRadioPlaybackState()

    assertSignalValue(model.radioPlaybackStateSignal,
      Some(Success(DummyUIModel.TestRadioPlaybackState))) flatMap { _ =>
      model.stopRadioPlayback()
      assertSignalValue(model.radioPlaybackStateSignal, Some(Success(expCurrentSource)))
    }
  }

  it should "handle a failed update" in {
    val exception = new IllegalStateException("Test exception when stopping playback.")
    val service = new RadioServiceTestImpl {
      override def stopPlayback(): Future[Unit] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.initRadioPlaybackState()
    model.stopRadioPlayback()

    assertSignalValue(model.radioPlaybackStateSignal, Some(Failure(exception)))
  }

  "changeRadioSource" should "change the current radio source successfully" in {
    val newSourceID = "theNewCurrentRadioSource"
    val newSourceVar: Var[String] = Var("")

    val service = new RadioService(ServiceUrl) {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.successful(DummyUIModel.CurrentSource)

      override def changeCurrentSource(id: String): Future[Unit] =
        newSourceVar set id
        Future.successful(())
    }

    val model = new DefaultUIModel(service)
    model.changeRadioSource(newSourceID)

    assertSignalValue(newSourceVar.signal, newSourceID)
  }

  it should "handle a failed change of the current radio source" in {
    val exception = new IllegalStateException("Test exception when changing the radio source.")
    val service = new RadioService(ServiceUrl) {
      override def changeCurrentSource(id: String): Future[Unit] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.changeRadioSource("someSourceID")

    assertSignalValue(model.radioPlaybackStateSignal, Some(Failure(exception)))
  }

  "shutdown" should "shutdown the player server" in {
    var shutdownCalls = 0
    val service = new RadioService(ServiceUrl) {
      override def shutdown(): Unit =
        shutdownCalls += 1
    }

    val model = new DefaultUIModel(service)
    model.shutdown()

    shutdownCalls should be(1)
  }

  it should "update the state of the current source" in {
    val service = new RadioService(ServiceUrl) {
      override def shutdown(): Unit = {}
    }

    val model = new DefaultUIModel(service)
    model.shutdown()

    Future {
      model.radioPlaybackStateSignal.asInstanceOf[StrictSignal[Option[Try[RadioService.CurrentSourceState]]]]
        .now() match
        case Some(Failure(exception)) =>
          exception shouldBe a[IllegalStateException]
        case v => fail("Unexpected value: " + v)
    }
  }

  "The event listener" should "process a radio message" in {
    //TODO: Testing of the event listener needs to be extended later. The current implementation is just a dummy
    //      to allow for manual testing of the web socket connection.
    val service = new RadioServiceTestImpl
    val model = new DefaultUIModel(service)
    model.initRadioPlaybackState()

    assertSignalValue(model.radioPlaybackStateSignal,
      Some(Success(DummyUIModel.TestRadioPlaybackState))) flatMap { _ =>
      val radioMessage = RadioModel.RadioMessage("someMessageType", "Some message payload.")
      val expectedState = DummyUIModel.TestRadioPlaybackState.copy(titleInfo = radioMessage.toString)
      service.sendRadioMessage(radioMessage)
      assertSignalValue(model.radioPlaybackStateSignal, Some(Success(expectedState)))
    }
  }

  it should "be registered only once" in {
    val service = new RadioServiceTestImpl
    val model = new DefaultUIModel(service)
    model.initRadioPlaybackState()

    assertSignalValue(model.radioPlaybackStateSignal,
      Some(Success(DummyUIModel.TestRadioPlaybackState))) flatMap { _ =>
      model.initRadioPlaybackState()
      val radioMessage = RadioModel.RadioMessage("someMessageType", "Some message payload.")
      val expectedState = DummyUIModel.TestRadioPlaybackState.copy(titleInfo = radioMessage.toString)
      Future {
        service.sendRadioMessage(radioMessage)
      } flatMap { _ =>
        assertSignalValue(model.radioPlaybackStateSignal, Some(Success(expectedState)))
      }
    }
  }

  /**
   * A test implementation of [[RadioService]] that provides some default
   * implementations for functions that are frequently used in tests.
   */
  private class RadioServiceTestImpl extends RadioService(ServiceUrl):
    /** Stores the listener function passed to ''registerEventListener()''. */
    private var listener: RadioModel.RadioMessage => Unit = _

    override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
      Future.successful(DummyUIModel.CurrentSource)

    override def registerEventListener(l: RadioModel.RadioMessage => Unit): Future[Unit] =
      if listener != null then
        // To test that this function is called only once, a second invocation resets the listener.
        listener = null
        l(RadioModel.RadioMessage("ERROR", "Unexpected registerEventListener() invocation"))
      else
        listener = l
      Future.successful(())

    /**
     * Makes sure that a listener function has been registered and invokes it
     * with the given message.
     *
     * @param message the message to pass to the listener
     */
    def sendRadioMessage(message: RadioModel.RadioMessage): Unit =
      listener should not be null
      listener(message)
  end RadioServiceTestImpl
