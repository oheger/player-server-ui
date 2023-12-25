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
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import scala.util.{Failure, Success, Try}

@main
def PlayerServerUi(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )
  Main.refreshUi()
end PlayerServerUi

object Main:
  /** The UI model. */
  private val uiModel = createUiModel()

  def appElement(): Element =
    div(
      className := "container",
      radioPlaybackStateElement(),
      favoritesElement(),
      radioSourcesElement()
    )
  end appElement

  /**
   * Performs a refresh of the UI state by loading all state information from
   * the server.
   *
   * @param model the current model
   */
  private[playerserverui] def refreshUi(model: UIModel = uiModel): Unit =
    model.initRadioSources()
    model.initRadioPlaybackState()

  /**
   * Returns an element that displays buttons to switch to favorite radio 
   * sources.
   *
   * @param model the UI model
   * @return the element to display favorite radio sources
   */
  private[playerserverui] def favoritesElement(model: UIModel = uiModel): Element =
    elementWithErrorAndLoadingIndicator(model.favoritesSignal, "favorites-wrapper") { favoritesSignal =>
      val favoritesElement = div(
        className := "favorites",
        children <-- favoritesSignal.split(_.id) { (id, _, sourceSignal) =>
          renderFavoriteSource(model, id, sourceSignal)
        }
      )

      Signal.fromValue(List(favoritesElement))
    }

  /**
   * Returns an element that displays the currently available radio sources.
   *
   * @param model the UI model
   * @return the element to display the radio sources
   */
  private[playerserverui] def radioSourcesElement(model: UIModel = uiModel): Element =
    elementWithErrorAndLoadingIndicator(model.sortedRadioSourcesSignal, "select-source") { sourcesSignal =>
      val rankingStepSignal = sourcesSignal.map(sources => (sources.map(_.ranking).max + 1) / 4.0)

      val sourcesElement = div(
        table(idAttr := "radioSourcesTable",
          tbody(
            children <-- sourcesSignal.split(_.id) { (id, _, sourceSignal) =>
              renderRadioSource(model, id, sourceSignal, rankingStepSignal)
            }
          )
        )
      )
      Signal.fromValue(List(sourcesElement))
    }

  /**
   * Returns an element that displays information about the current radio
   * playback state. The information of the state is displayed, together with
   * controls to update it.
   *
   * @param model the UI model
   * @return the element to display the current radio playback state
   */
  private[playerserverui] def radioPlaybackStateElement(model: UIModel = uiModel): Element =
    elementWithErrorAndLoadingIndicator(model.radioPlaybackStateSignal, "playback-state") { playbackStateSignal =>
      playbackStateSignal.map { currentRadioState =>

        def iconClass(stop: Boolean): String =
          val baseClass = "btnIcon"
          if currentRadioState.playbackEnabled == stop then baseClass else baseClass + " btnIconDisabled"

        currentRadioState.currentSource.fold(List.empty) { source =>
          val btnStartPlayback =
            radioControlButton("/playback-start.svg", "ctrl-btn-start",
              currentRadioState.playbackEnabled)(model.startRadioPlayback)
          val btnStopPlayback =
            radioControlButton("/playback-stop.svg", "ctrl-btn-stop",
              !currentRadioState.playbackEnabled)(model.stopRadioPlayback)
          val btnShutdown =
            radioControlButton("/shutdown.svg", "ctrl-btn-shutdown", inactive = false)(model.shutdown)

          val selectedSourceStyles = List("current-source")
          val selectedSourceReplacedStyles =
            if currentRadioState.replacementSource.isDefined then "current-source-replaced" :: selectedSourceStyles
            else selectedSourceStyles
          val divSelectedSource = radioSourceElement(Some(source), selectedSourceReplacedStyles)
          val divReplacementSource = radioSourceElement(currentRadioState.replacementSource,
            List("replacement-source"))
          val divTitle = div(
            className := "title-info",
            currentRadioState.titleInfo
          )
          val divDisplay = div(
            className := "radio-display",
            divSelectedSource,
            divReplacementSource,
            divTitle
          )

          List(
            divDisplay,
            btnStartPlayback,
            btnStopPlayback,
            btnShutdown
          )
        }
      }
    }

  /**
   * Generates an element for the specified radio source.
   *
   * @param model             the UI model
   * @param sourceID          the ID of the current radio source
   * @param sourceSignal      the signal for the radio source to be rendered
   * @param rankingStepSignal the signal for the mapping of the ranking to
   *                          style classes
   * @return the element representing this radio source
   */
  private def renderRadioSource(model: UIModel,
                                sourceID: String,
                                sourceSignal: Signal[RadioModel.RadioSource],
                                rankingStepSignal: Signal[Double]): Element =
    val rankingSignal = sourceSignal.map(_.ranking)
    val styleClassIdxSignal = for
      ranking <- rankingSignal
      rankingStep <- rankingStepSignal
    yield math.floor(ranking / rankingStep).toInt
    val iconClassSignal = styleClassIdxSignal.map(idx => s"radioSourceIcon$idx")
    val itemClassSignal = styleClassIdxSignal.map(idx => s"radioSourceItem$idx")
    val rankingTextSignal = rankingSignal.map(ranking => s"\u2606:$ranking")

    tr(className <-- itemClassSignal,
      onClick --> { _ => model.changeRadioSource(sourceID) },
      td(
        textAlign := "right",
        verticalAlign := "top",
        img(
          src := "/source-icon.svg",
          className <-- iconClassSignal
        )
      ),
      td(
        div(
          child.text <-- sourceSignal.map(_.name)
        ),
        div(
          className := "radioSourceRanking",
          child.text <-- rankingTextSignal
        )
      )
    )

  /**
   * Generates an element to display a favorite radio source.
   *
   * @param model        the UI model
   * @param sourceID     the ID of the favorite radio source
   * @param sourceSignal the signal for the source to be displayed
   * @return the element representing this favorite source
   */
  private def renderFavoriteSource(model: UIModel,
                                   sourceID: String,
                                   sourceSignal: Signal[RadioModel.RadioSource]): Element =
    button(
      className := "favorite-btn",
      onClick --> { _ => model.changeRadioSource(sourceID) },
      child.text <-- sourceSignal.map(_.name)
    )

  /**
   * Generates an element to display a radio source.
   *
   * @param source the source to be displayed
   * @param styles additional style classes to add
   * @return the element representing the radio source
   */
  private def radioSourceElement(source: Option[RadioModel.RadioSource], styles: List[String]): Element =
    div(
      className := ("radio-source" :: styles).mkString(" "),
      source.map(_.name) getOrElse ""
    )

  /**
   * Generates a button with an image that can be used to control the radio
   * player.
   *
   * @param image    the name of the image
   * @param style    an additional style class
   * @param handler  the handler function to handle button clicks
   * @param inactive flag whether the button is disabled
   * @return the button element
   */
  private def radioControlButton(image: String, style: String, inactive: Boolean)(handler: () => Unit): Element =
    button(
      className := s"ctrl-btn $style",
      onClick --> { _ => handler() },
      disabled := inactive,
      img(
        className := "ctrl-img",
        src := image
      )
    )

  /**
   * Creates an [[Element]] for a [[Signal]] that shows a loading or an error
   * indicator if needed. This function evaluates the given signal. If it is in
   * a special state, it produces corresponding UI elements. Otherwise, it
   * calls the given function to generate the UI for the normal state. In case
   * of an error state, an element is generated with the CSS class ''error''
   * plus another one starting with the given ''styleClass'' and the suffix
   * ''-error''.
   *
   * @param signal     the [[Signal]] to display
   * @param styleClass the CSS class for the new element
   * @param data       a function for handling the normal state
   * @tparam A the data type of the signal
   * @return an [[Element]] to display this signal
   */
  private def elementWithErrorAndLoadingIndicator[A](signal: Signal[Option[Try[A]]], styleClass: String)
                                                    (data: Signal[A] => Signal[List[Element]]): Element =
    div(
      className := styleClass,
      children <-- signal.flatMap {
        case None =>
          Signal.fromValue(List(loadingIndicator()))
        case Some(Failure(exception)) =>
          Signal.fromValue(List(p(className := s"error $styleClass-error", exception.getMessage)))
        case Some(Success(value)) =>
          data(Signal.fromValue(value))
      }
    )

  /**
   * Returns an [[Element]] to represent a loading indicator. This is used as
   * long as no real data is available yet for an element.
   *
   * @return the loading indicator element
   */
  private def loadingIndicator(): Element =
    div(className := "loading-indicator")

  /**
   * Creates the UI model for this application. Per default, a model
   * implementation is returned that is connected to the player server. If a
   * test environment is detected, a dummy model is returned instead.
   *
   * @return the [[UIModel]] for this application
   */
  private def createUiModel(): UIModel =
    fetchServerUri().filter(isProdEnvironment) map { serverUri =>
      new DefaultUIModel(new RadioService(baseUrl = serverUri))
    } getOrElse DummyUIModel

  /**
   * Tries to obtain the URI of the server from which this application was
   * loaded. This can fail in a test environment where the ''window'' object is
   * not available.
   *
   * @return a ''Try'' with the URI of the player server
   */
  private def fetchServerUri(): Try[String] = Try {
    dom.window.location.origin
  }

  /**
   * Checks whether the given server URI points to a test environment. This is
   * typically the case if the application was loaded from localhost.
   *
   * @param serverUri the server URI
   * @return a flag whether this URI indicates a test environment
   */
  private def isTestEnvironment(serverUri: String): Boolean =
    serverUri.contains("://localhost")

  /**
   * Checks whether the given server URI points to a production (i.e. a
   * non-test) environment.
   *
   * @param serverUri the server URI
   * @return a flag whether this URI indicates a real, production environment
   */
  private def isProdEnvironment(serverUri: String): Boolean = !isTestEnvironment(serverUri)
end Main
