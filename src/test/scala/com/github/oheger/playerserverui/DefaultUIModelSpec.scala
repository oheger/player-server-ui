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
import com.raquo.airstream.state.StrictSignal
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

    model.radioSourcesSignal.asInstanceOf[StrictSignal[Option[Try[RadioModel.RadioSources]]]].now() shouldBe empty
  }

  "initCurrentSource" should "fetch the current radio source from the server" in {
    val service = new RadioService(ServiceUrl) {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.successful(DummyUIModel.CurrentSource)
    }

    val model = new DefaultUIModel(service)
    model.initCurrentSource()

    assertSignalValue(model.currentSourceStateSignal, Some(Success(DummyUIModel.TestRadioPlaybackState)))
  }

  it should "record an exception when loading the current radio source from the server" in {
    val exception = new IllegalStateException("Test exception when loading the current radio source.")
    val service = new RadioService(ServiceUrl) {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.initCurrentSource()

    assertSignalValue(model.currentSourceStateSignal, Some(Failure(exception)))
  }

  it should "initially have a value of None" in {
    val model = new DefaultUIModel(new RadioService(ServiceUrl))

    model.currentSourceStateSignal.asInstanceOf[StrictSignal[Option[Try[RadioService.CurrentSourceState]]]]
      .now() shouldBe empty
  }

  "startRadioPlayback" should "update the playback state" in {
    val currentSourceState = DummyUIModel.CurrentSource.copy(playbackEnabled = false)
    val service = new RadioService(ServiceUrl) {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.successful(currentSourceState)

      override def startPlayback(): Future[Unit] =
        Future.successful(())
    }

    val model = new DefaultUIModel(service)
    model.initCurrentSource()
    model.startRadioPlayback()

    assertSignalValue(model.currentSourceStateSignal, Some(Success(DummyUIModel.TestRadioPlaybackState)))
  }

  it should "handle a failed update" in {
    val exception = new IllegalStateException("Test exception when starting playback.")
    val currentSourceState = DummyUIModel.CurrentSource.copy(playbackEnabled = false)
    val service = new RadioService(ServiceUrl) {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.successful(currentSourceState)

      override def startPlayback(): Future[Unit] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.initCurrentSource()
    model.startRadioPlayback()

    assertSignalValue(model.currentSourceStateSignal, Some(Failure(exception)))
  }

  "stopRadioPlayback" should "update the playback state" in {
    val expCurrentSource = DummyUIModel.TestRadioPlaybackState.copy(playbackEnabled = false)
    val service = new RadioService(ServiceUrl) {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.successful(DummyUIModel.CurrentSource)

      override def stopPlayback(): Future[Unit] =
        Future.successful(())
    }

    val model = new DefaultUIModel(service)
    model.initCurrentSource()
    model.stopRadioPlayback()

    assertSignalValue(model.currentSourceStateSignal, Some(Success(expCurrentSource)))
  }

  it should "handle a failed update" in {
    val exception = new IllegalStateException("Test exception when stopping playback.")
    val service = new RadioService(ServiceUrl) {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.successful(DummyUIModel.CurrentSource)

      override def stopPlayback(): Future[Unit] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.initCurrentSource()
    model.stopRadioPlayback()

    assertSignalValue(model.currentSourceStateSignal, Some(Failure(exception)))
  }

  "changeRadioSource" should "change the current radio source successfully" in {
    val newSourceID = "theNewCurrentRadioSource"
    val service = new RadioService(ServiceUrl) {
      override def loadCurrentSource(): Future[RadioService.CurrentSourceState] =
        Future.successful(DummyUIModel.CurrentSource)

      override def changeCurrentSource(id: String): Future[Unit] =
        if id == newSourceID then Future.successful(())
        else Future.failed(new IllegalArgumentException("Unexpected source ID: " + id))
    }

    val model = new DefaultUIModel(service)
    model.changeRadioSource(newSourceID)

    assertSignalValue(model.currentSourceStateSignal, Some(Success(DummyUIModel.TestRadioPlaybackState)))
  }

  it should "handle a failed change of the current radio source" in {
    val exception = new IllegalStateException("Test exception when changing the radio source.")
    val service = new RadioService(ServiceUrl) {
      override def changeCurrentSource(id: String): Future[Unit] =
        Future.failed(exception)
    }

    val model = new DefaultUIModel(service)
    model.changeRadioSource("someSourceID")

    assertSignalValue(model.currentSourceStateSignal, Some(Failure(exception)))
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
      model.currentSourceStateSignal.asInstanceOf[StrictSignal[Option[Try[RadioService.CurrentSourceState]]]]
        .now() match
        case Some(Failure(exception)) =>
          exception shouldBe a[IllegalStateException]
        case v => fail("Unexpected value: " + v)
    }
  }
