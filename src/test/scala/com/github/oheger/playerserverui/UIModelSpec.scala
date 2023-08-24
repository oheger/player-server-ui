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
import org.scalatest.compatible.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object UIModelSpec:
  /** Timeout when waiting for a specific signal value. */
  private val SignalTimeoutMs = 3000

/**
 * Test class for [[UIModel]].
 */
class UIModelSpec extends AsyncFlatSpec with Matchers:
  /**
   * The execution context for asynchronous checks of ScalaTest. This must be
   * the context used by Scala.js.
   */
  implicit override def executionContext: ExecutionContext =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  import UIModelSpec.*

  /**
   * Helper function for checking whether a signal takes a specific value. The
   * function checks asynchronously whether a value as specified by a condition
   * is found within a timeout. Otherwise, a failed assertion is returned.
   * @param signal the signal to test
   * @param test the function to check the signal value
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

  private def assertSignalValue[A](signal: Signal[A], expectedValue: A): Future[Assertion] =
    assertSignal(signal)(_ == expectedValue)

  "initRadioSources" should "fetch the radio sources from the server" in {
    val RadioSourcesList = List(
      RadioModel.RadioSource("s1", "RadioSource1", 1),
      RadioModel.RadioSource("s2", "RadioSource2", 2),
      RadioModel.RadioSource("s3", "RadioSource3", 3),
    )
    val service = new RadioService("https://radio.example.org") {
      override def loadRadioSources(): Future[RadioModel.RadioSources] =
        Future.successful(RadioModel.RadioSources(RadioSourcesList))
    }

    val model = new UIModel(service)
    model.initRadioSources()

    assertSignalValue(model.radioSourcesSignal, Success(RadioModel.RadioSources(RadioSourcesList)))
  }

  it should "record an exception when loading the radio sources from the server" in {
    val exception = new IllegalStateException("Test exception when loading radio sources.")
    val service = new RadioService("https://radio.example.org") {
      override def loadRadioSources(): Future[RadioModel.RadioSources] =
        Future.failed(exception)
    }

    val model = new UIModel(service)
    model.initRadioSources()

    assertSignalValue(model.radioSourcesSignal, Failure(exception))
  }
