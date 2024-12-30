/*
 * Copyright 2023-2024-2024 Oliver Heger.
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
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.client3.ziojson.*
import sttp.model.{StatusCode, Uri}
import sttp.ws.WebSocket
import zio.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

object RadioService:
  /**
   * A data class to represent the playback state for the current radio source.
   * This class collects the data from multiple calls of the player server REST
   * API: the current source status, and the playback state.
   *
   * @param optCurrentSourceID     the ID of the current radio source - if set
   * @param optReplacementSourceID the ID of the replacement source - if set
   * @param playbackEnabled        flag whether playback is currently active
   * @param optTitleInfo           optional information about the current title
   */
  final case class CurrentSourceState(optCurrentSourceID: Option[String],
                                      optReplacementSourceID: Option[String],
                                      playbackEnabled: Boolean,
                                      optTitleInfo: Option[String])

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
class RadioService(backend: SttpBackend[Future, WebSockets], baseUrl: String):
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
    val futCurrentSource = basicRequest.get(serverUri("/sources/current?full=true"))
      .response(asString.getRight)
      .send(backend).map { response =>
        response.code match
          case StatusCode.NoContent => None
          case _ => response.body.fromJson[RadioModel.RadioSourceStatus] match
            case Left(value) => throw new IllegalStateException("JSON decoding failed: " + value)
            case Right(value) => Some(value)
      }
    val futPlaybackState = basicRequest.get(serverUri("/playback"))
      .response(asJson[RadioModel.PlaybackStatus].getRight)
      .send(backend).map(_.body)

    for
      currentSource <- futCurrentSource
      playbackState <- futPlaybackState
    yield CurrentSourceState(currentSource flatMap (_.currentSourceId),
      currentSource flatMap (_.replacementSourceId),
      playbackState.enabled,
      currentSource flatMap (_.titleInfo))
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
   * Calls the server API to register an event listener for radio events. This
   * function expects a listener function that is invoked for each web socket
   * text message that was received. The content of the text message is
   * deserialized into a [[RadioModel.RadioMessage]] structure. Messages, for
   * which this is not possible, are ignored.
   *
   * @param listener the listener function
   * @return a ''Future'' with the outcome of this operation
   */
  def registerEventListener(listener: RadioModel.RadioMessage => Unit): Future[Unit] = mapException {
    val uri = s"ws${baseUrl.dropWhile(_ != ':')}/api/radio/events"
    basicRequest.get(uri"$uri")
      .response(asWebSocket(handleWebSocket(listener)).getRight)
      .send(backend)
      .map(_ => ())
  }

  /**
   * Generates a URI for calling the radio server API.
   *
   * @param subPath the sub path to be invoked
   * @return the resulting URI
   */
  private def serverUri(subPath: String): Uri =
    val uriStr = s"$baseUrl/api/radio$subPath"
    uri"$uriStr"

  /**
   * A function for handling web socket messages for a specific listener. This
   * function is passed to the web socket API of the ''STTP'' client. The
   * resulting ''Future'' indicates when the web socket connection can be
   * stopped. Since the connection should be active for the whole live-time of
   * this app, it never completes.
   *
   * @param listener the listener function to be invoked
   * @param ws       the [[WebSocket]] instance representing the connection
   * @return a ''Future'' to terminate the connection
   */
  private def handleWebSocket(listener: RadioModel.RadioMessage => Unit)(ws: WebSocket[Future]): Future[Unit] =
    def processMessage(): Unit =
      ws.receiveText().foreach { s =>
        s.fromJson[RadioModel.RadioMessage] match
          case Right(radioMessage) =>
            listener(radioMessage)
          case Left(err) =>
            println(s"Received an unexpected web socket message: '$s'. Error: '$err'.")

        processMessage()
      }

    processMessage()
    Promise[Unit]().future
