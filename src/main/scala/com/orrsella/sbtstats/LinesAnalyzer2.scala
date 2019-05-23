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
import org.apache.commons.io.FilenameUtils

/**
 * Lines count analyzer that measures every kind of file.
 *
 * Only works if files are stored in ascii-based encodings.
 *
 * Does not try to understand when lines are blank, comments, etc.
 */
class LinesAnalyzer2 extends Analyzer {

  def analyze(title: String, sources: Seq[File], packageBin: File, encoding: String) = {
    val withExt = sources.map { f =>
      val s = f.toString()
      if (s.endsWith(".tdml.xml")) // double extension .tdml.xml used in some places.
        ("tdml", f)
      else
        (FilenameUtils.getExtension(f.toString()), f)
    }
    val grouped = withExt.groupBy { _._1 }
    val byExt = grouped.map {
      case (key, listPairs) =>
        val files = listPairs.map { _._2 }
        val lines = files.map { f =>
          val csName = "ISO-8859-1" // will work to count lines for all ascii-based encodings.
          val count = Source.fromFile(f, csName).getLines.length
          count
        }
        val totalLines = lines.sum
        (key, listPairs.length, totalLines, files)
    }
    new LinesAnalyzer2Result(title, sources.length, byExt)
  }
}

class LinesAnalyzer2Result(
  titleArg: String,
  totalFiles: Int,
  byExt: Iterable[(String, Int, Int, Seq[File])])
  extends AnalyzerResult {

  override val title = titleArg + " lines"

  override lazy val metrics = byExt.toSeq.flatMap {
    case (ext, nFiles, nTotalLines, _) =>
      val extPlus = if (ext.length > 0) ext else "(no ext)"
      val pad = " " * (12 - extPlus.length)
      Seq(new AnalyzerMetric(title + " " + extPlus + pad, nTotalLines, "lines"))
  }
}
