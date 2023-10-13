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

import scala.util.Try

/**
 * A module defining classes to represent the data model of the player server
 * related to radio playback. Based on these classes, requests against the
 * server can be executed, and the payload of the responses can be interpreted.
 */
object RadioModel:
  /**
   * A data class representing the current playback status of the radio
   * player.
   *
   * @param enabled a flag whether playback is currently enabled
   */
  final case class PlaybackStatus(enabled: Boolean)

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

  /**
   * A data class representing the status of the radio source. It contains the
   * IDs of the currently selected source and a replacement source if defined.
   *
   * @param currentSourceId     the optional ID of the selected radio source
   * @param replacementSourceId the optional ID of the replacement source
   */
  final case class RadioSourceStatus(currentSourceId: Option[String],
                                     replacementSourceId: Option[String])

  /**
   * A data class defining messages sent from the server via a web socket
   * connection to represent radio events.
   *
   * Via this mechanism, the client can react on certain changes of the radio
   * playback state.
   *
   * @param messageType the type of the message as a string constant
   * @param payload     the payload of this message
   */
  final case class RadioMessage(messageType: String,
                                payload: String):
    /**
     * Returns an ''Option'' with the [[RadioMessageType]] that corresponds to
     * this message. If the message has a supported type, the resulting
     * ''Option'' is defined and can be evaluated; otherwise, it is ''None''.
     *
     * @return an ''Option'' with the [[RadioMessageType]] of this message
     */
    def radioMessageTypeOption: Option[RadioMessageType] =
      Try {
        RadioMessageType.valueOf(messageType)
      }.toOption

  /**
   * An enumeration class that defines the different types of radio messages.
   */
  enum RadioMessageType:
    case SourceChanged, ReplacementStart, ReplacementEnd, TitleInfo

  implicit val playbackStatusDecoder: JsonDecoder[PlaybackStatus] = DeriveJsonDecoder.gen[PlaybackStatus]
  implicit val radioSourceDecoder: JsonDecoder[RadioSource] = DeriveJsonDecoder.gen[RadioSource]
  implicit val radioSourcesDecoder: JsonDecoder[RadioSources] = DeriveJsonDecoder.gen[RadioSources]
  implicit val radioSourceStatusDecoder: JsonDecoder[RadioSourceStatus] = DeriveJsonDecoder.gen[RadioSourceStatus]
  implicit val radioMessageDecoder: JsonDecoder[RadioMessage] = DeriveJsonDecoder.gen[RadioMessage]

  /**
   * Constant for an unknown radio source. This constant is used when a radio
   * source ID cannot be resolved.
   */
  final val UnknownRadioSource = RadioSource("0", "Unknown", -1)
