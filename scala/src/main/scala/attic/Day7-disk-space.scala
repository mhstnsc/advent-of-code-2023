package attic

import attic.Day7.ProblemBase
import common.MainBase

object Day7 {
  class ProblemBase extends MainBase(7) {

    sealed trait Entry {
      val name: String
    }
    case class Dir(name: String, children: List[Entry])              extends Entry
    case class File(name: String, size: Long, parent: Option[Entry]) extends Entry

    def parseListing(parentDir: Dir, value: List[String]): List[Entry] = {
      val listDir  = "dir ([a-z]+)".r
      val listFile = "([0-9]+) ([a-z.]+)".r

      value match {
        case listDir(dirName) +: l     => Dir(dirName, Nil) +: parseListing(parentDir, l)
        case listFile(size, name) +: l => File(name, size.toLong, Some(parentDir)) +: parseListing(parentDir, l)
        case Nil                       => Nil
        case _                         => Nil
      }
    }

    def updateUpward(existingPath: List[Dir], newCurrentDir: Dir): List[Dir] =
      if (existingPath.size == 1) {
        List(newCurrentDir)
      } else {
        val parentDir = existingPath.dropRight(1).last

        val modifiedChildren = parentDir.children.map {
          case e @ Dir(_, _) if e.name == newCurrentDir.name => newCurrentDir
          case e @ _                                         => e
        }
        val newParentDir = parentDir.copy(children = modifiedChildren)
        updateUpward(existingPath.dropRight(1), newParentDir) :+ newCurrentDir
      }

    def parseTree(inputFile: List[String], currentPath: List[Dir]): Dir = {
      val cdSub = "\\$ cd ([a-z]+)".r

      val rootDir    = currentPath.head
      val currentDir = currentPath.last

      inputFile match {
        case "$ cd /" +: l => parseTree(l, List(rootDir))
        case "$ cd .." +: l =>
          if (currentPath.size == 1) {
            parseTree(l, currentPath)
          } else {
            parseTree(l, currentPath.dropRight(1))
          }
        case cdSub(targetName) +: l =>
          val existingChild = currentDir.children
            .find(e =>
              e match {
                case Dir(name, _) if name == targetName => true
                case _                                  => false
              }
            )
            .map {
              case d: Dir => d
              case _      => throw new Exception("should not happen to find a file")
            }

          existingChild.fold(
            {
              val childDir         = Dir(targetName, Nil)
              val newOldCurrentDir = currentDir.copy(children = currentDir.children :+ childDir)
              val newPath          = updateUpward(currentPath, newOldCurrentDir)
              parseTree(l, newPath)
            }
          ) { v =>
            parseTree(l, currentPath :+ v)
          }

        case "$ ls" +: l =>
          if (currentDir.children.isEmpty) {
            val newCurrentDir = currentDir.copy(children = parseListing(currentDir, inputFile.drop(1)))
            val newPath       = updateUpward(currentPath, newCurrentDir)
            parseTree(l, newPath)
          } else {
            // if we already listed then don't do anything
            parseTree(l, currentPath)
          }
        case _ +: l => parseTree(l, currentPath)
        case Nil    => rootDir
      }
    }

    def printTree(e: Entry, space: Long): Unit = {
      val fill = " ".repeat(space.toInt)
      e match {
        case Dir(name, children) =>
          println(s"${fill}[${name}]")
          children.foreach(e => printTree(e, space + 4))
        case File(name, size, _) =>
          println(s"$fill$name $size")
      }
    }

    def computeCumulativeDirSizes(dir: Dir): List[Long] = {
      val subdirSizes = dir.children.flatMap {
        case d: Dir => Some(computeCumulativeDirSizes(d))
        case _      => None
      }.flatten

      val fileSizes = dir.children.flatMap {
        case File(_, size, _) => Some(size)
        case _                => None
      }

      println(s"subdirsizes=${subdirSizes.sum} fileSizes=${fileSizes.sum} -> sum=${subdirSizes.sum + fileSizes.sum}")
      (subdirSizes.sum + fileSizes.sum) +: subdirSizes
    }

    def computeDirSizes(dir: Dir): (Long, List[Long]) = {
      val subdirSizes = dir.children.flatMap {
        case d: Dir => Some(computeDirSizes(d))
        case _      => None
      }

      val fileSizes = dir.children.flatMap {
        case File(_, size, _) => Some(size)
        case _                => None
      }

      val sizeOfThisDir = (subdirSizes.map(_._1).sum + fileSizes.sum)
      val r = (
        sizeOfThisDir,
        sizeOfThisDir +: subdirSizes.flatMap(_._2)
      )

      println(r)
      r
    }

    override def run(inputFile: List[String]): String = {
      val emptyRootDir = Dir("/", Nil)
      val rootDir      = parseTree(inputFile, List(emptyRootDir))
      printTree(rootDir, 0)
      computeCumulativeDirSizes(rootDir).filter(_ < 100000).sum.toString
    }
  }
}

object Day7Problem1 extends ProblemBase

object Day7Problem2 extends ProblemBase {

  override def run(inputFile: List[String]): String = {
    val emptyRootDir = Dir("/", Nil)
    val rootDir      = parseTree(inputFile, List(emptyRootDir))
    printTree(rootDir, 0)
    val totalDiskSize = 70000000
    val requiredUnused = 30000000
    val sizes = computeDirSizes(rootDir)._2.sorted
    val usedSpace = sizes.last
    val maybeDir = sizes.find  { group => usedSpace - group < totalDiskSize - requiredUnused }
    maybeDir.get.toString
  }

}
