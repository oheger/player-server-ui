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

  /**
   * A Var that indicates whether the radio playback state has already been
   * fetched.
   */
  private val radioPlaybackStateAvailableVar = Var(false)

  /**
   * A Var that indicates whether an exception occurred when fetching the
   * radio playback state.
   */
  private val radioPlaybackStateErrorVar: Var[Option[Throwable]] = Var(None)

  /**
   * A Var for storing the ID of the current radio source if available.
   */
  private val radioCurrentSourceIDVar: Var[Option[String]] = Var(None)

  /**
   * A Var for storing the ID of a replacement radio source while the
   * current source is disabled.
   */
  private val radioReplacementSourceIDVar: Var[Option[String]] = Var(None)

  /** A Var to track whether radio playback is currently enabled. */
  private val radioPlaybackEnabledVar = Var(false)

  /** A Var to store the current title information. */
  private val radioTitleInfoVar = Var("")

  /** A Var for storing the flag to display the radio source selection. */
  private val showRadioSourceSelectionVar = Var(false)

  /** Signal for the current list of radio sources. */
  override val radioSourcesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] = radioSourcesVar.signal

  override val radioPlaybackStateSignal: Signal[Option[Try[UIModel.RadioPlaybackState]]] =
    for
      available <- radioPlaybackStateAvailableVar.signal
      optError <- radioPlaybackStateErrorVar.signal
      optCurrent <- radioSourceFromIDSignal(radioCurrentSourceIDVar)
      optReplace <- radioSourceFromIDSignal(radioReplacementSourceIDVar)
      enabled <- radioPlaybackEnabledVar.signal
      title <- radioTitleInfoVar.signal
    yield
      if !available then None
      else
        optError match
          case Some(exception) => Some(Failure(exception))
          case None =>
            Some(Success(UIModel.RadioPlaybackState(optCurrent, optReplace, enabled, title)))

  /**
   * A signal used for internal purposes that generates a map from the list of
   * current radio sources using the source IDs as keys. This can be used to
   * obtain the radio source from an ID.
   */
  private val radioSourcesMapSignal = radioSourcesSignal.map {
    case Some(Success(sources)) =>
      sources.map(source => source.id -> source).toMap
    case _ => Map.empty
  }

  /**
   * A flag to record whether the listener for radio message has already been
   * registered. This is used to ensure that the listener is only registered
   * once.
   */
  private var radioMessageListenerRegistered = false

  override def showRadioSourceSelectionSignal: Signal[Boolean] = showRadioSourceSelectionVar.signal

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
      triedCurrentSource match
        case Success(state) =>
          radioPlaybackStateAvailableVar set true
          radioPlaybackStateErrorVar set None
          radioCurrentSourceIDVar set state.optCurrentSourceID
          radioReplacementSourceIDVar set state.optReplacementSourceID
          radioPlaybackEnabledVar set state.playbackEnabled
          radioTitleInfoVar set ""
        case Failure(exception) =>
          radioPlaybackStateAvailableVar set true
          radioPlaybackStateErrorVar set Some(exception)

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
    showRadioSourceSelection(visible = false)
    radioService.changeCurrentSource(sourceID) onComplete {
      case Success(_) => radioCurrentSourceIDVar set Some(sourceID)
      case Failure(exception) => radioPlaybackStateErrorVar set Some(exception)
    }

  override def showRadioSourceSelection(visible: Boolean): Unit =
    showRadioSourceSelectionVar set visible

  override def shutdown(): Unit =
    radioPlaybackStateErrorVar set Some(new IllegalStateException("Server is no longer available."))
    radioService.shutdown()

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
        radioPlaybackEnabledVar set playbackEnabled
      case Failure(exception) =>
        radioPlaybackStateErrorVar set Some(exception)

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
   *
   * @param message the radio message from the server
   */
  private def handleRadioMessage(message: RadioModel.RadioMessage): Unit =
    message.radioMessageTypeOption foreach {
      case RadioModel.RadioMessageType.SourceSelected =>
        radioCurrentSourceIDVar set Some(message.payload)
        radioReplacementSourceIDVar set None
      case RadioModel.RadioMessageType.SourceChanged =>
        radioPlaybackEnabledVar set true
      case RadioModel.RadioMessageType.ReplacementStart =>
        radioReplacementSourceIDVar set Some(message.payload)
      case RadioModel.RadioMessageType.ReplacementEnd =>
        radioReplacementSourceIDVar set None
      case RadioModel.RadioMessageType.TitleInfo =>
        radioTitleInfoVar set message.payload
      case RadioModel.RadioMessageType.PlaybackStopped =>
        radioPlaybackEnabledVar set false
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
        radioPlaybackStateErrorVar set Some(exception)
      case _ =>
        println("Radio message connection closed. Trying to reestablish it.")
        registerRadioMessageListener()

  /**
   * Returns a [[Signal]] that map the ID of the radio source in the given
   * [[Var]] to the referenced radio source. If the ID cannot be resolved, the
   * signal reports a dummy radio source.
   *
   * @param idVar the Var with the optional radio source ID
   * @return a signal with the optional resolved radio source
   */
  private def radioSourceFromIDSignal(idVar: Var[Option[String]]): Signal[Option[RadioModel.RadioSource]] =
    for
      optID <- idVar.signal
      sourceMap <- radioSourcesMapSignal
    yield
      optID map { id =>
        sourceMap.getOrElse(id, RadioModel.UnknownRadioSource)
      }
