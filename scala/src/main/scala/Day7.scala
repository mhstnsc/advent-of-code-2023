import Day7.ProblemBase
import common.MainBase

object Day7 {
  class ProblemBase extends MainBase(7) {

    sealed trait Entry {
      val name: String
      val parent: Option[Entry]
    }
    case class Dir(name: String, children: List[Entry], parent: Option[Dir]) extends Entry
    case class File(name: String, size: Long, parent: Option[Entry])         extends Entry

    def parseListing(parentDir: Dir, value: List[String]): List[Entry] = {
      val listDir  = "dir ([a-z]+)".r
      val listFile = "([0-9]+) ([a-z.]+)".r

      value match {
        case listDir(dirName) +: l     => Dir(dirName, Nil, Some(parentDir)) +: parseListing(parentDir, l)
        case listFile(size, name) +: l => File(name, size.toLong, Some(parentDir)) +: parseListing(parentDir, l)
        case Nil                       => Nil
        case _                         => Nil
      }
    }

    def updateUpward(dir: Dir): Dir =
      if (dir.parent.nonEmpty) {
        val modifiedChildren = dir.children.map {
          case e @ Dir(_, _, _) if e.name == dir.name => dir
          case e @ _                                  => e
        }
        val modifiedParent = dir.parent.get.copy(children = modifiedChildren)
        updateUpward(modifiedParent)
      } else {
        dir
      }

    def parseTree(inputFile: List[String], rootDir: Dir, currentDir: Dir): Dir = {
      val cdSub = "\\$ cd ([a-z]+)".r

      inputFile match {
        case "$ cd /" +: l => parseTree(l, rootDir, rootDir)
        case cdSub(targetName) +: l =>
          val existingChild = currentDir.children
            .find(e =>
              e match {
                case Dir(name, _, _) if name == targetName => true
                case _                                     => false
              }
            )
            .map {
              case d @ Dir(_, _, _) => d
              case _                => throw new Exception("should not happen to find a file")
            }

          existingChild.fold(
            {
              val childDir         = Dir(targetName, Nil, Some(currentDir))
              val newOldCurrentDir = currentDir.copy(children = currentDir.children :+ childDir)
              val newRoot          = updateUpward(newOldCurrentDir)
              parseTree(l, newRoot, childDir)
            }
          ) {
            v => parseTree(l, rootDir, v)
          }

        case "$ ls" +: l =>
          if (currentDir.children.isEmpty) {
            val newCurrentDir = currentDir.copy(children = parseListing(currentDir, inputFile.drop(1)))
            val newRoot       = updateUpward(newCurrentDir)
            parseTree(l, newRoot, newCurrentDir)
          } else {
            // if we already listed then don't do anything
            parseTree(l, rootDir, currentDir)
          }
        case _ +: l => parseTree(l, rootDir, currentDir)
        case Nil    => rootDir
      }
    }

    def printTree(e: Entry, space: Long): Unit = {
      val fill = " ".repeat(space.toInt)
      e match {
        case Dir(name, children, _) =>
          println(s"${fill}[${name}]")
          children.foreach(e => printTree(e, space + 4))
        case File(name, size, _) =>
          println(s"$fill$name $size")
      }
    }

    def computeDirSizes(dir: Dir): List[Long] = {
      val subdirSizes = dir.children.flatMap {
        case d: Dir => Some(computeDirSizes(d))
        case _      => None
      }.flatten

      val fileSizes = dir.children.flatMap {
        case File(_, size, _) => Some(size)
        case _                => None
      }

      (subdirSizes.sum + fileSizes.sum) +: subdirSizes
    }

    override def run(inputFile: List[String]): String = {
      val emptyRootDir = Dir("/", Nil, None)
      val rootDir      = parseTree(inputFile, emptyRootDir, emptyRootDir)
      printTree(rootDir, 0)
      computeDirSizes(rootDir).filter(_ < 10000).sum.toString
    }
  }
}

object Day6Problem1 extends ProblemBase

object Day6Problem2 extends ProblemBase {

  override def run(inputFile: List[String]): String =
    ""
}
