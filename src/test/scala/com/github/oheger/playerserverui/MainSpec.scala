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

  it should "initialize the current source" in {
    val model = new UIModelTestImpl

    Main.refreshUi(model)

    model.initRadioPlaybackStateCount should be(1)
  }

  "radioSourcesElement" should "display all available radio sources" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      $(element.ref).find("tr").length should be(DummyUIModel.DummyRadioSources.sources.size)
      DummyUIModel.DummyRadioSources.sources foreach { source =>
        $(element.ref).find(s"td:contains('${source.name}')").length should be(1)
      }
      $(element.ref).find("img[src='/loading.gif']").length should be(0)
    }
  }

  it should "order radio sources by their ranking per default" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      val elements = $(element.ref).find("tr")

      def assertSourceAt(index: Int, name: String): Unit =
        elements.get(index).get.textContent should include(name)

      assertSourceAt(0, "Rock Antenne")
      assertSourceAt(1, "SWR 1")
      assertSourceAt(6, "bar")
      assertSourceAt(7, "FOO")
    }
  }

  it should "ignore a failure in the current radio source" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    model setTriedRadioPlaybackState Failure(new IllegalStateException("Test exception"))
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      $(element.ref).find("tr").length should be(DummyUIModel.DummyRadioSources.sources.size)
    }
  }

  it should "filter out the current radio source" in {
    val selectedSource = DummyUIModel.DummyRadioSources.sources.head
    val currentSource = DummyUIModel.TestRadioPlaybackState.copy(currentSource = Some(selectedSource))
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    model setTriedRadioPlaybackState Success(currentSource)
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      $(element.ref).find("tr").length should be(DummyUIModel.DummyRadioSources.sources.size - 1)
      $(element.ref).find(s"tr:contains('${selectedSource.name}')").length should be(0)
    }
  }

  it should "use different styles based on the ranking of radio sources" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      val elements = $(element.ref).find("tr")

      def assertStyleClassAt(elementIndex: Int, classIndex: Int): Unit =
        val trElement = elements.get(elementIndex).get
        trElement.attributes.getNamedItem("class").value should be("radioSourceItem" + classIndex)
        trElement.getElementsByClassName("radioSourceIcon" + classIndex).length should be(1)

      assertStyleClassAt(0, 3)
      assertStyleClassAt(1, 3)
      assertStyleClassAt(2, 2)
      assertStyleClassAt(6, 0)
      assertStyleClassAt(7, 0)
    }
  }

  it should "display the ranking of radio sources" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      val elements = $(element.ref).find("tr")

      def assertRankingAt(index: Int, ranking: Int): Unit =
        val rankingStr = "\u2606:" + ranking
        elements.get(index).get.textContent should include(rankingStr)

      assertRankingAt(0, 9)
      assertRankingAt(1, 8)
      assertRankingAt(6, 0)
      assertRankingAt(7, 0)
    }
  }

  it should "show an error message if the sources could not be loaded" in {
    val message = "Failure while loading radio sources"
    val model = new UIModelTestImpl
    model setTriedRadioSources Failure(new IllegalStateException(message))
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      $(element.ref).find("p").length should be(1)
      $(element.ref).find(s"p.error:contains('$message')").length should be(1)
    }
  }

  it should "show a progress indicator while loading data" in {
    val element = Main.radioSourcesElement(new UIModelTestImpl)

    testDom(element) {
      $(element.ref).find("img[src='/loading.gif']").length should be(1)
    }
  }

  it should "allow switching to another radio source" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) {
      $(element.ref).find("tr").eq(1).trigger("click")

      model.newRadioSource should be("s1")
    }
  }

  "radioPlaybackStateElement" should "display the current radio source" in {
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(DummyUIModel.TestRadioPlaybackState)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) {
      val nodes = $(element.ref)
        .find(s"p:contains('${DummyUIModel.TestRadioPlaybackState.currentSource.get.name}')")
      nodes.length should be(1)
      $(element.ref).find("img[src='/playback-stop.svg']").length should be(1)
      $(element.ref).find("img[src='/playback-start.svg']").length should be(1)
      $(element.ref).find("img[src='/loading.gif']").length should be(0)
    }
  }

  it should "not display anything if no current source is selected" in {
    val currentSource = DummyUIModel.TestRadioPlaybackState.copy(currentSource = None)
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(currentSource)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) {
      $(element.ref).find("img[src='/playback-stop.png']").length should be(0)
      $(element.ref).find("img[src='/playback-start.png']").length should be(0)
    }
  }

  it should "show an error message if the current source could not be loaded" in {
    val message = "Failure while loading the current source"
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Failure(new IllegalStateException(message))
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) {
      $(element.ref).find(s"p.error:contains('$message')").length should be(1)
      $(element.ref).find("img[src='/playback-stop.png']").length should be(0)
      $(element.ref).find("img[src='/playback-start.png']").length should be(0)
      $(element.ref).find("img[src='/loading.gif']").length should be(0)
    }
  }

  it should "show a progress indicator while loading data" in {
    val element = Main.radioPlaybackStateElement(new UIModelTestImpl)

    testDom(element) {
      $(element.ref).find("img[src='/loading.gif']").length should be(1)
    }
  }

  it should "enable the button to start radio playback" in {
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(DummyUIModel.TestRadioPlaybackState.copy(playbackEnabled = false))
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) {
      $(element.ref).find("#btnStartRadioPlayback:enabled").trigger("click")

      model.startRadioPlaybackCount should be(1)

      $(element.ref).find("#btnStopRadioPlayback:disabled").length should be(1)
      $(element.ref).find("img.btnIconDisabled[src='/playback-start.svg']").length should be(0)
      $(element.ref).find("img.btnIconDisabled[src='/playback-stop.svg']").length should be(1)
    }
  }

  it should "enable the button to stop radio playback" in {
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(DummyUIModel.TestRadioPlaybackState)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) {
      $(element.ref).find("#btnStopRadioPlayback:enabled").trigger("click")

      model.stopRadioPlaybackCount should be(1)

      $(element.ref).find("#btnStartRadioPlayback:disabled").length should be(1)
      $(element.ref).find("img.btnIconDisabled[src='/playback-stop.svg']").length should be(0)
      $(element.ref).find("img.btnIconDisabled[src='/playback-start.svg']").length should be(1)
    }
  }

  it should "provide a button to shut down the player server" in {
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(DummyUIModel.TestRadioPlaybackState)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) {
      $(element.ref).find("#btnShutdown").trigger("click")

      model.shutdownCount should be(1)
    }
  }
end MainSpec

/**
 * A test implementation of [[UIModel]] for injecting defined test data into
 * the UI to be tested.
 */
private class UIModelTestImpl extends UIModel:
  /** Stores the current state of radio sources. */
  private val radioSources: Var[Option[Try[List[RadioModel.RadioSource]]]] = Var(None)

  /** Stores the radio playback source. */
  private val radioPlaybackState: Var[Option[Try[UIModel.RadioPlaybackState]]] = Var(None)

  /** A counter for the invocations of ''initRadioSources()''. */
  var initRadioSourcesCount = 0

  /** A counter for the invocations of ''initRadioPlaybackState()''. */
  var initRadioPlaybackStateCount = 0

  /** A counter for the invocations of ''startRadioPlayback()''. */
  var startRadioPlaybackCount = 0

  /** A counter for the invocations of stopRadioPlayback(). */
  var stopRadioPlaybackCount = 0

  /** A counter for the invocations of shutdown(). */
  var shutdownCount = 0

  /** Stores the updated radio source. */
  var newRadioSource = ""

  /**
   * Sets the value for the current radio sources. Here a ''Try'' can be
   * provided.
   *
   * @param triedSources the ''Try'' with the radio sources
   */
  def setTriedRadioSources(triedSources: Try[RadioModel.RadioSources]): Unit =
    radioSources set Some(triedSources.map(_.sources))

  /**
   * Sets the radio sources of this model to the given object.
   *
   * @param sources the new radio sources
   */
  def setRadioSources(sources: RadioModel.RadioSources): Unit =
    setTriedRadioSources(Success(sources))

  /**
   * Sets the data about the current radio playback state to the given object.
   *
   * @param triedState the ''Try'' with data about the radio playback source
   */
  def setTriedRadioPlaybackState(triedState: Try[UIModel.RadioPlaybackState]): Unit =
    radioPlaybackState set Some(triedState)

  override def radioSourcesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] = radioSources.signal

  override def radioPlaybackStateSignal: Signal[Option[Try[UIModel.RadioPlaybackState]]] = radioPlaybackState.signal

  override def initRadioSources(): Unit =
    initRadioSourcesCount += 1

  override def initRadioPlaybackState(): Unit =
    initRadioPlaybackStateCount += 1

  override def startRadioPlayback(): Unit =
    startRadioPlaybackCount += 1

  override def stopRadioPlayback(): Unit =
    stopRadioPlaybackCount += 1

  override def changeRadioSource(sourceID: String): Unit =
    newRadioSource = sourceID

  override def shutdown(): Unit =
    shutdownCount += 1
