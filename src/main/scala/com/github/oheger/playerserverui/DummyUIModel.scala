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
      RadioModel.RadioSource("s1", "SWR 1 BW", 8),
      RadioModel.RadioSource("s2", "Rock Antenne", 9),
      RadioModel.RadioSource("s3", "Energy", 1),
      RadioModel.RadioSource("s4", "Harmony FM", 2),
      RadioModel.RadioSource("s5", "Radio Active", 3),
      RadioModel.RadioSource("s6", "AFN", 2),
      RadioModel.RadioSource("s7", "FOO", 0),
      RadioModel.RadioSource("s8", "BAR", 0),
    )
  )

  /** A static data object with the dummy radio sources data. */
  private val sourcesVal = Val(Success(DummyRadioSources))

  override def radioSourcesSignal: Signal[Try[RadioModel.RadioSources]] = sourcesVal.signal

  override def initRadioSources(): Unit = {}
  