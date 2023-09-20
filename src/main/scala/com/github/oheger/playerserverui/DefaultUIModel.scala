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
import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Var

import scala.util.{Failure, Success, Try}

/**
 * A class storing the state of the player server UI.
 *
 * @param radioService the service to interact with the radio player
 */
class DefaultUIModel(radioService: RadioService) extends UIModel:
  /** Stores the current list of radio sources. */
  private val radioSourcesVar: Var[Option[Try[List[RadioModel.RadioSource]]]] = Var(None)

  /** Stores the state of the current radio source. */
  private val currentSourceVar: Var[Option[Try[RadioService.CurrentSourceState]]] = Var(None)

  /** Signal for the current list of radio sources. */
  override val radioSourcesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] = radioSourcesVar.signal

  override val currentSourceStateSignal: Signal[Option[Try[RadioService.CurrentSourceState]]] =
    currentSourceVar.signal

  import scala.concurrent.ExecutionContext.Implicits.global

  /**
   * Loads the current list of radio sources from the server using the
   * [[RadioService]].
   */
  override def initRadioSources(): Unit =
    radioService.loadRadioSources() onComplete { triedSources =>
      radioSourcesVar.set(Some(triedSources.map(_.sources)))
    }

  override def initCurrentSource(): Unit =
    radioService.loadCurrentSource() onComplete { triedCurrentSource =>
      currentSourceVar.set(Some(triedCurrentSource))
    }

  override def startRadioPlayback(): Unit =
    radioService.startPlayback() onComplete { res =>
      updatePlaybackState(res, playbackEnabled = true)
    }

  override def stopRadioPlayback(): Unit =
    radioService.stopPlayback() onComplete { res =>
      updatePlaybackState(res, playbackEnabled = false)
    }

  override def changeRadioSource(sourceID: String): Unit =
    radioService.changeCurrentSource(sourceID) onComplete {
      case Success(_) => initCurrentSource()
      case Failure(exception) => currentSourceVar set Some(Failure(exception))
    }

  override def shutdown(): Unit =
    currentSourceVar set Some(Failure(new IllegalStateException("Server is no longer available.")))
    radioService.shutdown()

  /**
   * Changes the playback state to the given value. This function is called
   * when the user has started or stopped playback.
   *
   * @param triedResult     the outcome of the change operation
   * @param playbackEnabled the new playback enabled state
   */
  private def updatePlaybackState(triedResult: Try[Unit], playbackEnabled: Boolean): Unit =
    triedResult match
      case Success(_) =>
        currentSourceVar.update { optState =>
          optState.map { triedState =>
            triedState.map(_.copy(playbackEnabled = playbackEnabled))
          }
        }
      case Failure(exception) =>
        currentSourceVar set Some(Failure(exception))
