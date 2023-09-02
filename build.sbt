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

lazy val VersionLaminar = "15.0.1"
lazy val VersionScala = "3.3.0"
lazy val VersionScalaJsDom = "2.4.0"
lazy val VersionSttp = "3.9.0"

lazy val VersionJqueryFacade = "2.1"
lazy val VersionJsDom = "21.1.1"
lazy val VersionScalaTest = "3.2.16"
lazy val VersionWebpack = "5.76.2"

lazy val playerServerUi = project.in(file("."))
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
  .settings(
    scalaVersion := VersionScala,
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % VersionScalaJsDom,
    libraryDependencies += "com.raquo" %%% "laminar" % VersionLaminar,
    libraryDependencies += "com.softwaremill.sttp.client3" %%% "core" % VersionSttp,
    libraryDependencies += "com.softwaremill.sttp.client3" %%% "zio-json" % VersionSttp,
    libraryDependencies += "org.scalatest" %%% "scalatest" % VersionScalaTest % Test,
    libraryDependencies += "org.querki" %%% "jquery-facade" % VersionJqueryFacade % Test,

    (Test / npmDependencies) += "jquery" -> "3.7.1",
    (Test / requireJsDomEnv) := true,
    (installJsdom / version) := VersionJsDom,
    (webpack / version) := VersionWebpack,
  )
