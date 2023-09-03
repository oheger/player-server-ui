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

import scala.util.Try

/**
 * A trait defining functionality to update and query the state of the player
 * server UI.
 */
trait UIModel:
  /**
   * Returns a signal for the current list of radio sources. The signal can
   * also report a failure when querying the radio sources from the server. It
   * has the value ''None'' while data is loaded from the server.
   *
   * @return the signal with the current state of available radio sources
   */
  def radioSourcesSignal: Signal[Option[Try[RadioModel.RadioSources]]]

  /**
   * Loads the current list of radio sources from the server. This function
   * should be invoked on application startup.
   */
  def initRadioSources(): Unit
