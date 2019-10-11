object AAAA extends App {
  override def main(args: Array[String]) {
    println(InstructionsParser.ParseAll("PUSHINT 3\nPUSHINT 3"))
  }
}
