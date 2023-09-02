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
import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import org.querki.jquery.*
import org.scalajs.dom
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
 * Test class for the main UI object.
 */
class MainSpec extends AnyFlatSpec with Matchers:
  /**
   * Executes a test on DOM elements. The function mounts the given element
   * into the DOM and triggers Laminar to render it. Then it executes the given
   * block to run tests on it.
   *
   * @param element the element to be tested
   * @param block   the test function
   */
  private def testDom(element: Element)(block: => Unit): Unit =
    val container = dom.document.createElement("div")
    dom.document.body.appendChild(container)
    try
      L.render(container, element)
      block
    finally
      dom.document.body.textContent = "" // This removes the container element.

  "refreshUi" should "initialize the radio sources" in {
    val model = new UIModelTestImpl

    Main.refreshUi(model)

    model.initRadioSourcesCount should be(1)
  }

  "radioSourcesElement" should "display all available radio sources" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      $(element.ref).find("p").length should be(DummyUIModel.DummyRadioSources.sources.size)
      DummyUIModel.DummyRadioSources.sources foreach { source =>
        $(element.ref).find(s"p:contains('${source.name}')").length should be(1)
      }
    }
  }

  it should "show an error message if the sources could not be loaded" in {
    val message = "Failure while loading radio sources"
    val model = new UIModelTestImpl
    model setTriedRadioSources Failure(new IllegalStateException(message))
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      $(element.ref).find("p").length should be(1)
      $(element.ref).find(s"p.error:contains('$message')")
    }
  }
end MainSpec

/**
 * A test implementation of [[UIModel]] for injecting defined test data into
 * the UI to be tested.
 */
private class UIModelTestImpl extends UIModel:
  /** Stores the current state of radio sources. */
  private val radioSources: Var[Try[RadioModel.RadioSources]] = Var(Success(RadioModel.RadioSources(List.empty)))

  /** A counter for the invocations of ''initRadioSources()''. */
  var initRadioSourcesCount = 0

  /**
   * Sets the value for the current radio sources. Here a ''Try'' can be
   * provided.
   *
   * @param triedSources the ''Try'' with the radio sources
   */
  def setTriedRadioSources(triedSources: Try[RadioModel.RadioSources]): Unit =
    radioSources set triedSources

  /**
   * Sets the radio sources of this model to the given object.
   *
   * @param sources the new radio sources
   */
  def setRadioSources(sources: RadioModel.RadioSources): Unit =
    setTriedRadioSources(Success(sources))

  override def radioSourcesSignal: Signal[Try[RadioModel.RadioSources]] = radioSources.signal

  override def initRadioSources(): Unit =
    initRadioSourcesCount += 1