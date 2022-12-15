package common

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object FileUtils {
  def writeFile(filename: String, contents: String) =
    Files.write(Paths.get("result.txt"), contents.getBytes(StandardCharsets.UTF_8))
}
