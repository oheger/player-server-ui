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

import com.github.oheger.playerserverui.UIModel.RadioSourcesSortMode.Ranking
import com.github.oheger.playerserverui.model.RadioModel
import com.github.oheger.playerserverui.service.RadioService
import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Val

import scala.util.{Success, Try}

/**
 * An implementation of the [[UIModel]] trait that returns static dummy data.
 * It is mainly used for testing and development purposes.
 */
object DummyUIModel extends UIModel:
  /** A static object with radio sources. */
  final val DummyRadioSources: RadioModel.RadioSources = RadioModel.RadioSources(
    List(
      RadioModel.RadioSource("s1", "SWR 1 BW", 8, favoriteIndex = Some(1), favoriteName = Some("SWR1")),
      RadioModel.RadioSource("s2", "Rock Antenne", 9),
      RadioModel.RadioSource("s3", "Energy", 1, favoriteIndex = Some(-1), favoriteName = Some("foo")),
      RadioModel.RadioSource("s4", "Harmony FM", 4),
      RadioModel.RadioSource("s5", "Radio Active", 5, favoriteIndex = Some(3), favoriteName = Some("ACTIVE")),
      RadioModel.RadioSource("s6", "AFN", 2, favoriteIndex = Some(2)),
      RadioModel.RadioSource("s7", "FOO", 0),
      RadioModel.RadioSource("s8", "bar", 0),
    )
  )

  /**
   * An object defining the state of the current radio source as reported by 
   * the radio service.
   */
  final val CurrentSource = RadioService.CurrentSourceState(
    Some(DummyRadioSources.sources(4).id),
    None,
    playbackEnabled = true,
    None
  )

  /** An object defining test data for the radio playback state. */
  final val TestRadioPlaybackState = UIModel.RadioPlaybackState(
    currentSource = Some(DummyRadioSources.sources(4)),
    replacementSource = None,
    playbackEnabled = CurrentSource.playbackEnabled,
    titleInfo = ""
  )

  /** Another object with test data that defines all properties. */
  final val TestRadioPlaybackStateEnhanced = TestRadioPlaybackState.copy(
    replacementSource = Some(DummyRadioSources.sources.head),
    titleInfo = "Dire Straits - Brothers in Arms"
  )

  /** A static data object with the dummy radio sources data. */
  private val sourcesVal = Val(Some(Success(DummyRadioSources.sources)))

  /** A static data object with the current radio playback state. */
  private val radioPlaybackStateVal = Val(Some(Success(TestRadioPlaybackStateEnhanced)))

  override def radioSourcesSignal: Signal[Option[Try[List[RadioModel.RadioSource]]]] = sourcesVal.signal

  override val radioPlaybackStateSignal: Signal[Option[Try[UIModel.RadioPlaybackState]]] =
    radioPlaybackStateVal.signal

  override val showRadioSourceSelectionSignal: Signal[Boolean] = Signal.fromValue(false)

  override val radioSourcesSortModeSignal: Signal[UIModel.RadioSourcesSortMode] = Signal.fromValue(Ranking)

  override def initRadioSources(): Unit = {}

  override def initRadioPlaybackState(): Unit = {}

  override def startRadioPlayback(): Unit = {}

  override def stopRadioPlayback(): Unit = {}

  override def changeRadioSource(sourceID: String): Unit = {}

  override def showRadioSourceSelection(visible: Boolean): Unit = {}

  override def setRadioSourcesSortMode(mode: UIModel.RadioSourcesSortMode): Unit = {}

  override def shutdown(): Unit = {}
