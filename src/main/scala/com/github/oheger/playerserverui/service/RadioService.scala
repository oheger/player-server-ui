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
import com.github.oheger.playerserverui.service.RadioService.mapException
import sttp.client3.*
import sttp.client3.ziojson.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object RadioService:
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
class RadioService(backend: SttpBackend[Future, Any], baseUrl: String) {
  def this(baseUrl: String) = this(FetchBackend(), baseUrl)

  /**
   * Loads the currently available radio sources.
   *
   * @return a ''Future'' with the radio sources
   */
  def loadRadioSources(): Future[RadioModel.RadioSources] = mapException {
    basicRequest.get(uri"$baseUrl/api/sources")
      .response(asJson[RadioModel.RadioSources].getRight)
      .send(backend).map(_.body)
  }
}
