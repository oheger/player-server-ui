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

  /** Stores the current radio playback state. */
  private val radioPlaybackStateVar: Var[Option[Try[UIModel.RadioPlaybackState]]] = Var(None)

  /** Signal for the current list of radio sources. */
  override val radioSourcesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] = radioSourcesVar.signal

  override val radioPlaybackStateSignal: Signal[Option[Try[UIModel.RadioPlaybackState]]] =
    radioPlaybackStateVar.signal

  /**
   * A flag to record whether the listener for radio message has already been
   * registered. This is used to ensure that the listener is only registered
   * once.
   */
  private var radioMessageListenerRegistered = false

  import scala.concurrent.ExecutionContext.Implicits.global

  /**
   * Loads the current list of radio sources from the server using the
   * [[RadioService]].
   */
  override def initRadioSources(): Unit =
    radioService.loadRadioSources() onComplete { triedSources =>
      radioSourcesVar.set(Some(triedSources.map(_.sources)))
    }

  override def initRadioPlaybackState(): Unit =
  // To avoid race conditions, the service calls are intentionally done sequentially.
    radioService.loadCurrentSource() onComplete { triedCurrentSource =>
      val radioPlaybackState = triedCurrentSource map { source =>
        UIModel.RadioPlaybackState(currentSource = source.optCurrentSource,
          replacementSource = None,
          playbackEnabled = source.playbackEnabled,
          titleInfo = "")
      }
      radioPlaybackStateVar.set(Some(radioPlaybackState))

      if !radioMessageListenerRegistered then
        registerRadioMessageListener()
        // Setting the variable here should hopefully be safe in JavaScript.
        radioMessageListenerRegistered = true
    }

  override def startRadioPlayback(): Unit =
    radioService.startPlayback() onComplete { res =>
      updateRadioPlaybackEnabledState(res, playbackEnabled = true)
    }

  override def stopRadioPlayback(): Unit =
    radioService.stopPlayback() onComplete { res =>
      updateRadioPlaybackEnabledState(res, playbackEnabled = false)
    }

  override def changeRadioSource(sourceID: String): Unit =
    radioService.changeCurrentSource(sourceID) onComplete {
      case Success(_) => initRadioPlaybackState()
      case Failure(exception) => radioPlaybackStateVar set Some(Failure(exception))
    }

  override def shutdown(): Unit =
    radioPlaybackStateVar set Some(Failure(new IllegalStateException("Server is no longer available.")))
    radioService.shutdown()

  /**
   * Updates the value of the current radio playback state. This function
   * takes care of calling the ''map()'' functions on the ''Option'' and the
   * ''Try'' that wrap the radio state. If a value is available, the given
   * update function is invoked.
   *
   * @param update the function to update the state
   */
  private def updateRadioPlaybackState(update: UIModel.RadioPlaybackState => UIModel.RadioPlaybackState): Unit =
    radioPlaybackStateVar.update { optState =>
      optState.map { triedState =>
        triedState.map(update)
      }
    }

  /**
   * Changes the radio playback enabled state to the given value. This function
   * is called when the user has started or stopped playback.
   *
   * @param triedResult     the outcome of the change operation
   * @param playbackEnabled the new playback enabled state
   */
  private def updateRadioPlaybackEnabledState(triedResult: Try[Unit], playbackEnabled: Boolean): Unit =
    triedResult match
      case Success(_) =>
        updateRadioPlaybackState { state =>
          state.copy(playbackEnabled = playbackEnabled)
        }
      case Failure(exception) =>
        radioPlaybackStateVar set Some(Failure(exception))

  /**
   * Registers the listener for radio messages. The registration yields a
   * ''Future'' that completes when the underlying web socket connection is no
   * longer available. This function registers a completion function that
   * evaluates the result the connection future was completed with.
   */
  private def registerRadioMessageListener(): Unit =
    val futRegistration = radioService.registerEventListener(handleRadioMessage)
    futRegistration onComplete radioMessageConnectionCompleted

  /**
   * The function to handle [[RadioModel.RadioMessage]]s received from the
   * server. Depending on the message, the radio state is updated accordingly.
   * TODO: This is currently a dummy implementation.
   *
   * @param message the radio message from the server
   */
  private def handleRadioMessage(message: RadioModel.RadioMessage): Unit =
    updateRadioPlaybackState { state =>
      state.copy(titleInfo = message.toString)
    }

  /**
   * A function that is called when the web socket connection for the radio
   * events is closed.
   *
   * @param result the value of the completed future
   */
  private def radioMessageConnectionCompleted(result: Try[Unit]): Unit =
    result match
      case Failure(exception) =>
        radioPlaybackStateVar set Some(Failure(exception))
      case _ =>
        println("Radio message connection closed. Trying to reestablish it.")
        registerRadioMessageListener()
