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
end PlayerServerUi

object Main:
  /** The UI model. */
  private val uiModel = createUiModel()

  def appElement(): Element =
    div(
      h1("PlayerServer UI"),
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

  /**
   * Returns an element that displays the currently available radio sources.
   *
   * @param model the UI model
   * @return the element to display the radio sources
   */
  private[playerserverui] def radioSourcesElement(model: UIModel = uiModel): Element =
    div(
      children <-- model.radioSourcesSignal.map {
        case Some(Success(sources)) =>
          sources.sources.map(renderRadioSource)
        case Some(Failure(exception)) =>
          List(p(className := "error", exception.getMessage))
        case None =>
          List(img(src := "/loading.gif", alt := "Loading"))
      }
    )

  /**
   * Generates an element for the specified radio source.
   *
   * @param source the radio source to be rendered
   * @return the element representing this radio source
   */
  private def renderRadioSource(source: RadioModel.RadioSource): Element =
    p(source.name)

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
