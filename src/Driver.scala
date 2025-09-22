package demo

import java.io.{File, FileWriter}
import scala.collection.mutable.ArrayBuffer

object Driver {
  val result = ArrayBuffer[String]()
  val ir = ArrayBuffer[DefModule]()

  var currentModule: Option[Module] = None

  def dump_port(p: Port): String = {
    s"    ${p.id.dir.get} [${p.id.width-1}:0] ${p.id.name}"
  }

  def dump_inst(module: Module): String = {
    val result = ArrayBuffer[String]()
    val ports = module.ports.toSeq
    result += s"    ${module.name} ${module.inst_name} (\n"
    ports foreach { p =>
      val port_conn = p.id.connect match {
        case Some(c) =>
          s"        .${p.id.name} ( ${c.name} )"
        case None =>
          s"        .${p.id.name} ( )"
      }

      if (p != ports.last) {
        result += s"${port_conn},\n"
      } else {
        result += s"${port_conn}\n"
      }
    }
    result += s"    );\n"
    result.toSeq.mkString
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
      m.cmds foreach { c =>
        c match {
          case DefVar(id) =>
            result += s"    wire [${id.width-1}:0] ${id.name};\n"
          case DefInstance(id) =>
            result += dump_inst(id)
        }
      }
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
