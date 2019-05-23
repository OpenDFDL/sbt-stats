/**
 * Copyright (c) 2013 Orr Sella
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

package com.orrsella.sbtstats

import java.io.File
import sbt.Extracted
import sbt.AutoPlugin
import sbt.Project
import sbt.Command
import sbt.util.Logger
import sbt.State
import sbt.EvaluateTask
import sbt.Value
import sbt.SettingKey
import sbt.TaskKey
import sbt.Compile
import sbt.Test
import sbt.settingKey
import sbt.taskKey
import sbt.Keys
import sbt.Keys.sources
import sbt.Keys.resources
import sbt.Keys.packageBin
import sbt.Keys.commands
import sbt.Keys.name
import sbt.Keys.state
import sbt.Keys.aggregate

object StatsPlugin extends AutoPlugin {

  object autoImport {
    lazy val statsAnalyzers = settingKey[Seq[Analyzer]]("stats-analyzers")
    lazy val statsDisplayProject = taskKey[Unit]("Prints code statistics for a single project, the current one")
    lazy val statsAnalyzer = taskKey[Seq[AnalyzerResult]]("Returns code statistics for a project, without printing it (shouldn't be used directly)")
    lazy val statsEncoding = settingKey[String]("stats-encoding")
  }

  import autoImport._

  def getFileTree(f: File): Stream[File] =
    if (!f.exists()) Stream()
    else if (f.isDirectory()) f.listFiles().toStream.flatMap(getFileTree)
    else Stream(f)

  lazy val statsSettings = Seq(
    commands += statsCommand,
    statsAnalyzers := Seq(
      new FilesAnalyzer(),
      new LinesAnalyzer(),
      new CharsAnalyzer(),
      new LinesAnalyzer2()),
    statsDisplayProject := statsDisplayProjectTask(statsAnalyzer.value, name.value, state.value.log),
    statsAnalyzer :=
      statsAnalyzerTask(
        statsAnalyzers.value,
        (resources in Compile).value,
        (sources in Compile).value,
        (resources in Test).value,
        (sources in Test).value,
        (packageBin in Compile).value,
        statsEncoding.value, state.value.log),
    statsEncoding := "UTF-8",
    aggregate in statsDisplayProject := false,
    aggregate in statsAnalyzer := false)

  override def trigger = allRequirements
  override lazy val projectSettings = statsSettings ++ Seq(Keys.commands ++= Seq(statsCommand))

  lazy val statsCommand = Command.command("stats") { state => doCommand(state) }

  private def doCommand(state: State): State = {
    val log = state.log
    val extracted: Extracted = Project.extract(state)
    val structure = extracted.structure
    val projectRefs = structure.allProjectRefs

    val results: Seq[AnalyzerResult] = projectRefs.flatMap {
      projectRef =>
        EvaluateTask(structure, statsAnalyzer, state, projectRef) match {
          case Some((state, Value(seq))) => seq
          case _ => Seq()
        }
    }

    val distinctTitles = results.map(_.title).distinct
    val summedResults = distinctTitles.map(t => results.filter(r => r.title == t).reduceLeft(_ + _))

    log.info("")
    log.info("Code Statistics for project:")
    log.info("")

    summedResults.foreach(res => {
      log.info(res.toString)
      log.info("")
    })

    // return unchanged state
    state
  }

  private def statsDisplayProjectTask(
    results: Seq[AnalyzerResult],
    name: String,
    log: Logger) {
    results.foreach(res => {
      log.info("")
      log.info("Code Statistics for project '" + name + "':")
      log.info("")
      log.info(res.toString)
      log.info("")
    })
  }

  private def statsAnalyzerTask(
    analyzers: Seq[Analyzer],
    resources: Seq[File],
    sources: Seq[File],
    testResources: Seq[File],
    testSources: Seq[File],
    packageBin: File,
    encoding: String,
    log: Logger) = {
    val statsResources = resources.flatMap { f => getFileTree(f) }.distinct
    val statsSources = sources.flatMap { f => getFileTree(f) }.distinct
    val statsTestResources = testResources.flatMap { f => getFileTree(f) }.distinct
    val statsTestSources = testSources.flatMap { f => getFileTree(f) }.distinct
    val srcResults =
      for (a <- analyzers) yield a.analyze("Source", statsSources ++ statsResources, packageBin, encoding)
    val tstResults =
      for (a <- analyzers) yield a.analyze("Test", statsTestSources ++ statsTestResources, packageBin, encoding)
    srcResults ++ tstResults
  }
}
