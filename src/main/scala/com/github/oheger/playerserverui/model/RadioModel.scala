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

package com.github.oheger.playerserverui.model

import zio.json.{DeriveJsonDecoder, JsonDecoder}

/**
 * A module defining classes to represent the data model of the player server
 * related to radio playback. Based on these classes, requests against the
 * server can be executed, and the payload of the responses can be interpreted.
 */
object RadioModel:
  /**
   * A data class representing a radio source.
   *
   * @param id      an internal ID assigned to the radio source
   * @param name    the name of the radio source
   * @param ranking a ranking
   */
  final case class RadioSource(id: String,
                               name: String,
                               ranking: Int)

  /**
   * A data class representing the list of [[RadioSource]] objects currently
   * available to the radio player.
   *
   * @param sources the list with existing radio sources
   */
  final case class RadioSources(sources: List[RadioSource])

  implicit val radioSourceDecoder: JsonDecoder[RadioSource] = DeriveJsonDecoder.gen[RadioSource]
  implicit val radioSourcesDecoder: JsonDecoder[RadioSources] = DeriveJsonDecoder.gen[RadioSources]
  
