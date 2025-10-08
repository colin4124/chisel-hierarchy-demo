package demo

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox

object instantiableMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    def processBody(stats: Seq[Tree]): (Seq[Tree], Iterable[Tree]) = {
      val extensions = scala.collection.mutable.ArrayBuffer.empty[Tree]
      val resultStats = stats.flatMap { stat =>
        stat match {
          case aVal: ValDef if aVal.mods.annotations.toString.contains("new public()") =>
            extensions += atPos(aVal.pos)(q"def ${aVal.name} = ___module._lookup(_.${aVal.name})")
            Seq(aVal)
          case other => Seq(other)
        }
      }
      (resultStats, extensions)
    }
    val result = {
      val (clz, objOpt) = annottees.map(_.tree).toList match {
        case Seq(c, o) => (c, Some(o))
        case Seq(c)    => (c, None)
        case _ =>
          throw new Exception("ERROR")
      }
      val (newClz, implicitClzs, tpname) = clz match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
          val instname = TypeName(tpname.toString + c.freshName())
          val (newStats, extensions) = processBody(stats)
          val argTParams = tparams.map(_.name)
          (
            q""" $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$newStats } """,
            Seq(
              q"""implicit class $instname[..$tparams](___module: Instance[$tpname[..$argTParams]]) { ..$extensions } """
            ),
            tpname
          )
        case _ =>
          throw new Exception("ERROR")
      }
      val newObj = objOpt match {
        case None => q"""object ${tpname.toTermName} { ..$implicitClzs } """
        case Some(obj @ q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }") =>
          q"""
            $mods object $tname extends { ..$earlydefns } with ..$parents { $self =>
              ..$implicitClzs
              ..$body
            }
          """
        case _ =>
          throw new Exception("ERROR")
      }
      q"""
        $newClz

        $newObj
      """
    }
    c.Expr[Any](result)
  }
}

class instantiable extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro instantiableMacro.impl
}
class public extends StaticAnnotation
