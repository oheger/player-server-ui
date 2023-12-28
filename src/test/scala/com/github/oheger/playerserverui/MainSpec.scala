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
import org.scalajs.dom.Element as DomElement
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
   * block to run tests on it passing the container element as argument.
   *
   * @param element the element to be tested
   * @param block   the test function
   */
  private def testDom(element: Element)(block: DomElement => Unit): Unit =
    val container = dom.document.createElement("div")
    dom.document.body.appendChild(container)
    try
      L.render(container, element)
      block(container)
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

    testDom(element) { container =>
      $(container).find("li").length should be(DummyUIModel.DummyRadioSources.sources.size)
      DummyUIModel.DummyRadioSources.sources foreach { source =>
        $(container).find(s"li:contains('${source.name}')").length should be(1)
      }
      $(container).find("div[class='loading-indicator']").length should be(0)
    }
  }

  it should "order radio sources by their ranking per default" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) { container =>
      val elements = $(container).find("li")

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

    testDom(element) { container =>
      $(container).find("li").length should be(DummyUIModel.DummyRadioSources.sources.size)
    }
  }

  it should "filter out the current radio source" in {
    val selectedSource = DummyUIModel.DummyRadioSources.sources.head
    val currentSource = DummyUIModel.TestRadioPlaybackState.copy(currentSource = Some(selectedSource))
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    model setTriedRadioPlaybackState Success(currentSource)
    val element = Main.radioSourcesElement(model)

    testDom(element) { container =>
      $(container).find("li").length should be(DummyUIModel.DummyRadioSources.sources.size - 1)
      $(container).find(s"li:contains('${selectedSource.name}')").length should be(0)
    }
  }

  it should "display the ranking of radio sources" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) { container =>
      val elements = $(container).find("li")

      def assertRankingAt(index: Int, ranking: Int): Unit =
        val rankingStr = "\u2606:" + ranking
        elements.get(index).get.textContent should include(rankingStr)

      assertRankingAt(0, 9)
      assertRankingAt(1, 8)
      assertRankingAt(6, 0)
      assertRankingAt(7, 0)
    }
  }

  it should "allow switching to another radio source" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) { container =>
      $(container).find("li").eq(1).trigger("click")

      model.newRadioSource should be("s1")
    }
  }

  it should "add a button to close the selection" in {
    val model = new UIModelTestImpl
    model setRadioSources DummyUIModel.DummyRadioSources
    val element = Main.radioSourcesElement(model)

    testDom(element) { container =>
      $(container).find(".source-selection-close-btn").trigger("click")

      model.showRadioSourceSelectionFlag should be(Some(false))
    }
  }

  "radioSourceSelectionCheckboxElement" should "generate a checkbox to control the display of radio sources" in {
    val model = new UIModelTestImpl
    val element = Main.radioSourceSelectionCheckboxElement(model)

    testDom(element) { container =>
      $(container).find(s"#${Main.CheckBoxRadioSourceSelection}").length should be(1)
      $(container).find(s"#${Main.CheckBoxRadioSourceSelection}:checked").length should be(0)
    }
  }

  it should "connect the checkbox to the UI model" in {
    val model = new UIModelTestImpl
    model.setShowRadioSourceSelection(true)
    val element = Main.radioSourceSelectionCheckboxElement(model)

    testDom(element) { container =>
      $(container).find(s"#${Main.CheckBoxRadioSourceSelection}:checked").length should be(1)
    }
  }

  "radioPlaybackStateElement" should "display the current radio source" in {
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(DummyUIModel.TestRadioPlaybackState)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) { container =>
      val nodes = $(element.ref)
        .find(s"div.current-source:contains('${DummyUIModel.TestRadioPlaybackState.currentSource.get.name}')")
      nodes.length should be(1)
      $(container).find("img[src='/playback-stop.svg']").length should be(1)
      $(container).find("img[src='/playback-start.svg']").length should be(1)
      $(container).find("div[class='loading-indicator']").length should be(0)
    }
  }

  it should "not display anything if no current source is selected" in {
    val currentSource = DummyUIModel.TestRadioPlaybackState.copy(currentSource = None)
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(currentSource)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) { container =>
      $(container).find("img[src='/playback-stop.png']").length should be(0)
      $(container).find("img[src='/playback-start.png']").length should be(0)
    }
  }

  it should "show an error message if the current source could not be loaded" in {
    val message = "Failure while loading the current source"
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Failure(new IllegalStateException(message))
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) { container =>
      $(container).find(s"p.error:contains('$message')").length should be(1)
      $(container).find("img[src='/playback-stop.png']").length should be(0)
      $(container).find("img[src='/playback-start.png']").length should be(0)
      $(container).find("div[class='loading-indicator']").length should be(0)
    }
  }

  it should "show a progress indicator while loading data" in {
    val element = Main.radioPlaybackStateElement(new UIModelTestImpl)

    testDom(element) { container =>
      $(container).find("div[class='loading-indicator']").length should be(1)
    }
  }

  it should "enable the button to start radio playback" in {
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(DummyUIModel.TestRadioPlaybackState.copy(playbackEnabled = false))
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) { container =>
      $(container).find("button.ctrl-btn-start:enabled").trigger("click")

      model.startRadioPlaybackCount should be(1)

      $(container).find("button.ctrl-btn-stop:disabled").length should be(1)
    }
  }

  it should "enable the button to stop radio playback" in {
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(DummyUIModel.TestRadioPlaybackState)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) { container =>
      $(container).find("button.ctrl-btn-stop:enabled").trigger("click")

      model.stopRadioPlaybackCount should be(1)

      $(container).find("button.ctrl-btn-start:disabled").length should be(1)
    }
  }

  it should "provide a button to shut down the player server" in {
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(DummyUIModel.TestRadioPlaybackState)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) { container =>
      $(container).find("button.ctrl-btn-shutdown").trigger("click")

      model.shutdownCount should be(1)
    }
  }

  it should "display title information" in {
    val TitleInfo = "Test Artist / Test Song"
    val radioState = DummyUIModel.TestRadioPlaybackState.copy(titleInfo = TitleInfo)
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(radioState)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) { container =>
      $(container).find(s"div.title-info:contains('$TitleInfo')").length should be(1)
    }
  }

  it should "display a replacement source" in {
    val replacementSource = DummyUIModel.DummyRadioSources.sources(5)
    val radioState = DummyUIModel.TestRadioPlaybackState.copy(replacementSource = Some(replacementSource))
    val model = new UIModelTestImpl
    model setTriedRadioPlaybackState Success(radioState)
    val element = Main.radioPlaybackStateElement(model)

    testDom(element) { container =>
      $(container).find(s"div.replacement-source:contains('${replacementSource.name}')").length should be(1)
    }
  }

  "favoritesElement" should "display buttons for all favorite radio sources" in {
    val favorites = List(
      RadioModel.RadioSource("f1", "Fav1", 1),
      RadioModel.RadioSource("f2", "Fav2", 1),
      RadioModel.RadioSource("f3", "Favorite3", 1)
    )
    val model = new UIModelTestImpl
    model setTriedFavoriteSources Success(favorites)
    val element = Main.favoritesElement(model)

    testDom(element) { container =>
      favorites.zipWithIndex foreach { (source, index) =>
        $(container).find(s".favorite-btn").eq(index).text() should be(source.name)
      }
    }
  }

  it should "show an error message if the favorites could not be loaded" in {
    val message = "Failure while loading radio sources"
    val model = new UIModelTestImpl
    model setTriedFavoriteSources Failure(new IllegalStateException(message))
    val element = Main.favoritesElement(model)

    testDom(element) { container =>
      $(container).find(s"p.error:contains('$message')").length should be(1)
      $(container).find(".favorite-btn").length should be(0)
    }
  }

  it should "show a progress indicator while loading data" in {
    val element = Main.favoritesElement(new UIModelTestImpl)

    testDom(element) { container =>
      $(container).find("div[class='loading-indicator']").length should be(1)
    }
  }

  it should "switch to another source when clicking a favorite button" in {
    val model = new UIModelTestImpl
    model setTriedFavoriteSources Success(DummyUIModel.DummyRadioSources.sources.take(2))
    val element = Main.favoritesElement(model)

    testDom(element) { container =>
      $(container).find("button").eq(1).trigger("click")

      model.newRadioSource should be("s2")
    }
  }

  it should "generate a button to select a radio source" in {
    val model = new UIModelTestImpl
    model setTriedFavoriteSources Success(DummyUIModel.DummyRadioSources.sources.take(2))
    val element = Main.favoritesElement(model)

    testDom(element) { container =>
      $(container).find("button").eq(2).trigger("click")

      model.showRadioSourceSelectionFlag should be(Some(true))
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

  /** Stores the list of favorite radio sources. */
  private val favoriteSources: Var[Option[Try[List[RadioModel.RadioSource]]]] = Var(None)

  /** Stores the radio playback source. */
  private val radioPlaybackState: Var[Option[Try[UIModel.RadioPlaybackState]]] = Var(None)

  /** Stores the flag whether to show the radio source selection. */
  private val radioSourceSelection = Var(false)

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

  /** Stores the flag passed to the showRadioSourceSelection() function. */
  var showRadioSourceSelectionFlag: Option[Boolean] = None

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
   * Sets the value for the favorite sources of this model.
   *
   * @param triedFavorites the ''Try'' with the favorite sources
   */
  def setTriedFavoriteSources(triedFavorites: Try[List[RadioModel.RadioSource]]): Unit =
    favoriteSources set Some(triedFavorites)

  /**
   * Sets the data about the current radio playback state to the given object.
   *
   * @param triedState the ''Try'' with data about the radio playback source
   */
  def setTriedRadioPlaybackState(triedState: Try[UIModel.RadioPlaybackState]): Unit =
    radioPlaybackState set Some(triedState)

  /**
   * Sets the value of the signal for the radio source selection.
   *
   * @param visible the new signal value
   */
  def setShowRadioSourceSelection(visible: Boolean): Unit =
    radioSourceSelection set visible

  override def radioSourcesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] = radioSources.signal

  override def favoritesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] = favoriteSources.signal

  override def radioPlaybackStateSignal: Signal[Option[Try[UIModel.RadioPlaybackState]]] = radioPlaybackState.signal

  override def showRadioSourceSelectionSignal: Signal[Boolean] = radioSourceSelection.signal

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

  override def showRadioSourceSelection(visible: Boolean): Unit =
    showRadioSourceSelectionFlag = Some(visible)

  override def shutdown(): Unit =
    shutdownCount += 1
