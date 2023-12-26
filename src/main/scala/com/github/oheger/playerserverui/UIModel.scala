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

import com.github.oheger.playerserverui.UIModel.{RadioPlaybackState, mapOptionalErrorSignal}
import com.github.oheger.playerserverui.model.RadioModel
import com.raquo.airstream.core.Signal

import scala.util.{Failure, Success, Try}

object UIModel:
  /**
   * A data class representing the current radio playback state. The class
   * contains all the information available for the radio player that could be
   * of interest for the UI.
   *
   * @param currentSource     the currently selected radio source if any
   * @param replacementSource the replacement source if any
   * @param playbackEnabled   a flag whether playback is currently enabled
   * @param titleInfo         a string with current tile information (empty if
   *                          not available)
   */
  final case class RadioPlaybackState(currentSource: Option[RadioModel.RadioSource],
                                      replacementSource: Option[RadioModel.RadioSource],
                                      playbackEnabled: Boolean,
                                      titleInfo: String)

  /**
   * Helper function to apply a mapping on a signal that contains an optional
   * value and can indicate a failure. This function handles mapping of the
   * ''Optional'' and the ''Try''; the provided mapping function is only
   * applied on normal values.
   *
   * @param signal the signal to map
   * @param f      the mapping function on normal signal values
   * @tparam A the data type of the signal
   * @tparam B the result type of the mapping function
   * @return the resulting mapped signal
   */
  private def mapOptionalErrorSignal[A, B](signal: Signal[Option[Try[A]]])(f: A => B): Signal[Option[Try[B]]] =
    signal.map { opt =>
      opt.map { triedValue =>
        triedValue map f
      }
    }
end UIModel

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
  def radioSourcesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]]

  /**
   * Returns a signal with a list of radio sources that can be selected by the
   * user. This is the list of available radio sources without the current
   * radio source.
   *
   * @return the signal with the radio sources that can be selected
   */
  def selectableRadioSourcesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] =
    optCurrentSourceIDSignal flatMap { optCurrentID =>
      mapOptionalErrorSignal(radioSourcesSignal) { sources =>
        sources filterNot { source => optCurrentID contains source.id }
      }
    }

  /**
   * Returns a signal for the current list of selectable radio sources (i.e.
   * excluding the current radio source) that is sorted by the criteria
   * selected by the user. Currently, the list of radio sources is ordered by
   * ranking; later, other sort criteria may be supported.
   *
   * @return the signal with the sorted list of radio sources
   */
  def sortedRadioSourcesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] =
    mapOptionalErrorSignal(selectableRadioSourcesSignal) { sources =>
      sources.sortWith { (src1, src2) =>
        (src1.ranking > src2.ranking) ||
          (src1.ranking == src2.ranking && src1.name.compareToIgnoreCase(src2.name) < 0)
      }
    }

  /**
   * Returns a signal for the playback state of the current radio source. This
   * can be used to display the current radio source and whether playback is
   * currently active. The signal supports default loading and error handling.
   *
   * @return the signal with the current source state
   */
  def radioPlaybackStateSignal: Signal[Option[Try[RadioPlaybackState]]]

  /**
   * Returns a signal with a sorted list of the favorite radio sources. The
   * sources in the resulting list are already ordered correctly and have been
   * assigned the favorite name.
   *
   * @return the signal with the list of favorite sources
   */
  def favoritesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] =
    mapOptionalErrorSignal(radioSourcesSignal) { sources =>
      sources.filter(_.favoriteIndex.exists(_ >= 0))
        .sortBy(_.favoriteIndex.get)
        .map { source => source.favoriteName.fold(source) { name => source.copy(name = name) } }
    }

  /**
   * Returns a signal that indicates whether the selection of radio sources
   * should be displayed. The list of all radio sources available is shown only
   * on demand. This signal determines whether it is visible or not.
   *
   * @return the signal controlling the display of the list of radio sources
   */
  def showRadioSourceSelectionSignal: Signal[Boolean]

  /**
   * Loads the current list of radio sources from the server. This function
   * should be invoked on application startup.
   */
  def initRadioSources(): Unit

  /**
   * Loads the state for the radio playback from the server. This causes the
   * signal for this state to be updated.
   */
  def initRadioPlaybackState(): Unit

  /**
   * Sends a command to the server to start radio playback.
   */
  def startRadioPlayback(): Unit

  /**
   * Sends a command to the server to stop radio playback.
   */
  def stopRadioPlayback(): Unit

  /**
   * Switches to another current radio source. Triggers a corresponding API
   * call and updates the current source.
   *
   * @param sourceID the ID of the new radio source
   */
  def changeRadioSource(sourceID: String): Unit

  /**
   * Sets the flag whether the list of available radio sources should be
   * displayed. This function is called as a reaction to user interactions with
   * certain UI controls.
   *
   * @param visible a flag whether the radio source selection should be shown
   *                or not
   */
  def showRadioSourceSelection(visible: Boolean): Unit

  /**
   * Shuts down the player server. Also updates the current state to indicate
   * that the server is no longer available.
   */
  def shutdown(): Unit

  /**
   * Returns a signal with the name of the currently selected radio source if
   * it is defined.
   *
   * @return a signal with the name of the current radio source
   */
  private def optCurrentSourceIDSignal: Signal[Option[String]] =
    radioPlaybackStateSignal.map { optSource =>
      optSource.flatMap { triedSource =>
        triedSource match
          case Failure(exception) => None
          case Success(value) => value.currentSource.map(_.id)
      }
    }
