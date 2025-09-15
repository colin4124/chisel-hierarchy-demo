package demo

import java.io.{File, FileWriter}
import scala.collection.mutable.ArrayBuffer

object Driver {
  val result = ArrayBuffer[String]()
  val ir = ArrayBuffer[DefModule]()

  def dump_port(p: Port): String = {
    s"    ${p.id.dir.get} [${p.id.width-1}:0] ${p.id.name}"
  }

  def dump_ir(): Unit = {
    ir foreach { case DefModule(m) =>
      result += s"module ${m.name} (\n"
      m.ports foreach { p =>
        if (p != m.ports.last) {
          result += dump_port(p) + ",\n"
        } else {
          result += dump_port(p) + "\n"
        }
      }
      result += s");\n"
      result += s"endmodule\n"
    }
  }

  def apply(m: Module, path: String): Unit = {
    dump_ir()

    val fileWriter = new FileWriter(new File(path))

    result foreach {
      fileWriter.write(_)
    }

    fileWriter.close()
  }
}
