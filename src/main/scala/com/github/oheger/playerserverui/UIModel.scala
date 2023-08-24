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
import com.raquo.airstream.state.{StrictSignal, Var}

import scala.util.{Success, Try}

/**
 * A class storing the state of the player server UI.
 * @param radioService the service to interact with the radio player
 */
class UIModel(radioService: RadioService):
  /** Stores the current list of radio sources. */
  private val radioSourcesVar: Var[Try[RadioModel.RadioSources]] = Var(Success(RadioModel.RadioSources(List.empty)))

  /** Signal for the current list of radio sources. */
  val radioSourcesSignal: StrictSignal[Try[RadioModel.RadioSources]] = radioSourcesVar.signal

  import scala.concurrent.ExecutionContext.Implicits.global

  /**
   * Loads the current list of radio sources from the server using the
   * [[RadioService]].
   */
  def initRadioSources(): Unit =
    radioService.loadRadioSources() onComplete radioSourcesVar.set
