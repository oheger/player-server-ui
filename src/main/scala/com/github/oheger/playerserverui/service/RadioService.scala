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
import com.github.oheger.playerserverui.service.RadioService.{CurrentSourceState, mapException}
import sttp.client3.*
import sttp.client3.ziojson.*
import sttp.model.{StatusCode, Uri}
import zio.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object RadioService:
  /**
   * A data class to represent the playback state for the current radio source.
   * This class collects the data from multiple calls of the player server REST
   * API: the currently played source, and the playback state.
   *
   * @param optCurrentSource the current radio source - if set
   * @param playbackEnabled  flag whether playback is currently active
   */
  final case class CurrentSourceState(optCurrentSource: Option[RadioModel.RadioSource],
                                      playbackEnabled: Boolean)

  /**
   * Helper function to improve the error handling for future results. If a
   * request fails with a non-success status code, this causes a rather
   * meaningless exception. Only the cause of this exception contains the
   * actual error code. This function therefore maps the exception to the cause
   * if possible.
   *
   * @param futResult the result future to be mapped if it failed
   * @tparam T the type of the result
   * @return the mapped future
   */
  private def mapException[T](futResult: Future[T]): Future[T] =
    futResult recoverWith {
      case e if e.getCause.isInstanceOf[HttpError[_]] =>
        Future.failed(e.getCause)
    }
end RadioService

/**
 * A service that handles the interaction with the player server backend to
 * play radio.
 *
 * An instance is initialized with an HTTP backend for sending REST requests
 * and the URL to reach the server. It provides methods for interacting with
 * the radio player that are translated to corresponding REST requests.
 *
 * @param backend the backend for sending HTTP requests
 * @param baseUrl the base URL of the player server
 */
class RadioService(backend: SttpBackend[Future, Any], baseUrl: String):
  def this(baseUrl: String) = this(FetchBackend(), baseUrl)

  /**
   * Loads the currently available radio sources.
   *
   * @return a ''Future'' with the radio sources
   */
  def loadRadioSources(): Future[RadioModel.RadioSources] = mapException {
    basicRequest.get(serverUri("/sources"))
      .response(asJson[RadioModel.RadioSources].getRight)
      .send(backend).map(_.body)
  }

  /**
   * Loads information about the current radio source and the playback status.
   *
   * @return a ''Future'' with information about the current source
   */
  def loadCurrentSource(): Future[CurrentSourceState] = mapException {
    val futCurrentSource = basicRequest.get(serverUri("/sources/current"))
      .response(asString.getRight)
      .send(backend).map { response =>
        response.code match
          case StatusCode.NoContent => None
          case _ => response.body.fromJson[RadioModel.RadioSource] match
            case Left(value) => throw new IllegalStateException("JSON decoding failed: " + value)
            case Right(value) => Some(value)
      }
    val futPlaybackState = basicRequest.get(serverUri("/playback"))
      .response(asJson[RadioModel.PlaybackStatus].getRight)
      .send(backend).map(_.body)

    for
      currentSource <- futCurrentSource
      playbackState <- futPlaybackState
    yield CurrentSourceState(currentSource, playbackState.enabled)
  }

  /**
   * Calls the server API to start radio playback and returns a ''Future'' to
   * check whether this was successful.
   *
   * @return a ''Future'' with the outcome of this operation
   */
  def startPlayback(): Future[Unit] = mapException {
    basicRequest.post(serverUri("/playback/start"))
      .response(asString.getRight)
      .send(backend) map { _ => () }
  }

  /**
   * Calls the server API to stop radio playback and returns a ''Future'' to
   * check whether this was successful.
   *
   * @return a ''Future'' with the outcome of this operation
   */
  def stopPlayback(): Future[Unit] = mapException {
    basicRequest.post(serverUri("/playback/stop"))
      .response(asString.getRight)
      .send(backend) map { _ => () }
  }

  /**
   * Calls the server API to switch to another radio source and returns a
   * ''Future'' to indicate whether this action was successful. The ID of the
   * new current radio source must be provided.
   *
   * @param id the ID of the new radio source
   * @return a ''Future'' with the outcome of this operation
   */
  def changeCurrentSource(id: String): Future[Unit] = mapException {
    basicRequest.post(serverUri(s"/sources/current/$id"))
      .response(asString.getRight)
      .send(backend).map { _ => () }
  }

  /**
   * Calls the server API to shutdown the server.
   */
  def shutdown(): Unit =
    basicRequest.post(uri"$baseUrl/api/shutdown")
      .send(backend)

  /**
   * Generates a URI for calling the radio server API.
   *
   * @param subPath the sub path to be invoked
   * @return the resulting URI
   */
  private def serverUri(subPath: String): Uri =
    val uriStr = s"$baseUrl/api/radio$subPath"
    uri"$uriStr"
