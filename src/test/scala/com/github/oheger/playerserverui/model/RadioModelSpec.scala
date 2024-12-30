/*
 * Copyright 2023-2024 Oliver Heger.
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

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.json.*

/**
 * Test class for some of the functionality provided by [[RadioModel]].
 */
class RadioModelSpec extends AnyFlatSpec with Matchers with EitherValues:
  "RadioMessage" should "return a supported radio message type" in {
    RadioModel.RadioMessageType.values foreach { messageType =>
      val messagePayload =
        s"""
           |{
           |  "messageType": "$messageType",
           |  "payload": "test payload"
           |}""".stripMargin

      messagePayload.fromJson[RadioModel.RadioMessage] match
        case Left(error) =>
          fail("JSON conversion failed with message: " + error)
        case Right(message) =>
          message should be(RadioModel.RadioMessage(messageType.toString, "test payload"))
          message.radioMessageTypeOption should be(Some(messageType))
    }
  }

  it should "return None for an unsupported radio message type" in {
    val message = RadioModel.RadioMessage("unknownType", "somePayload")

    message.radioMessageTypeOption shouldBe empty
  }
