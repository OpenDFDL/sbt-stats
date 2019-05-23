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
import scala.io.Source

class CharsAnalyzer extends Analyzer {
  override def analyze(title: String, sources: Seq[File], packageBin: File, encoding: String) = {
    val lines: Seq[Line] = for {
      file <- sources
      line <- Source.fromFile(file, "iso-8859-1").getLines
    } yield new Line(line)

    val totalChars = lines.map(_.length).foldLeft(0)(_ + _)
    val codeChars = lines.filter(_.isCode).map(_.length).foldLeft(0)(_ + _)
    val commentChars = lines.filter(_.isComment).map(_.length).foldLeft(0)(_ + _)

    new CharAnalyzerResult(title, totalChars, codeChars, commentChars)
  }
}

class CharAnalyzerResult(
  titleArg: String,
  totalChars: Int,
  codeChars: Int,
  commentChars: Int) extends AnalyzerResult {

  override val title = "Chars " + titleArg

  val metrics =
    Seq(
      new AnalyzerMetric("Total:     ", totalChars, "chars"),
      new AnalyzerMetric("Code:      ", codeChars, totalChars, "chars"),
      new AnalyzerMetric("Comment:   ", commentChars, totalChars, "chars"))
}
