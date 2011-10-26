object Ruler {
  def main(args: Array[String]): Unit = {
    ruler(height = 6, sections = 2)
  }
  def ruler(height: Int, sections: Int) = {
    def pipes(pipeCount: Int, spaceCount: Int): String = {
      val spaces = 0.until(spaceCount).map(s => " ").mkString
      0.until(pipeCount).map(s => "|").mkString(spaces)
    }
    var pipeCount = (sections / 2.0) + 1
    for (i <- 0 until height) {
      pipeCount = (pipeCount * 2) - 1
      val spaceCount = Math.pow(2, (height - i - 1)).toInt - 1
      println(pipes(pipeCount.toInt, spaceCount))
    }
  }
}

// ruler(height = 6, sections = 2) gives:
// 
// 
// |                               |                               |
// |               |               |               |               |
// |       |       |       |       |       |       |       |       |
// |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
// | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
// |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
