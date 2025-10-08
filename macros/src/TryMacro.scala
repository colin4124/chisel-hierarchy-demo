package foo

import scala.language.experimental.macros
import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.reflect.macros.whitebox

object tryMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def processBody(stats: Seq[Tree]): (Seq[Tree], Iterable[Tree]) = {
      val extensions = scala.collection.mutable.ArrayBuffer.empty[Tree]
      val resultStats = stats.flatMap { stat =>
        stat match {
          case hasPublic: ValOrDefDef if hasPublic.mods.annotations.toString.contains("new public()") =>
            hasPublic match {
              case aDef: DefDef =>
                val args_name = aDef.vparamss map { x => x map { _.name } }
                extensions += atPos(stat.pos)(q"${aDef.mods} def ${aDef.name}(...${aDef.vparamss}) = x._lookup(_.${aDef.name}(...${args_name}))")
                Seq(aDef)
              case aVal: ValDef =>
                extensions += atPos(stat.pos)(q"${aVal.mods} def ${aVal.name} = x._lookup(_.${aVal.name})")
                Seq(aVal)
            }
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
          throw new Exception(
            s"Only support annotation at class or object! Match error: annottees.map(_.tree).toList=${annottees.map(_.tree).toList}"
          )
      }
      val (implicitClzs, tpname) = clz match {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
          /** Definition Name */
          // Base on orignal name and add fresh name for unique
          val defname = TypeName(tpname.toString + c.freshName())

          /** Instance Name */
          // c.freshName() is diff when call it each time, so instname is diff for defname
          val instname = TypeName(tpname.toString + c.freshName())

          val (newStats, extensions) = processBody(stats)
          val argTParams = tparams.map(_.name)
          (
            Seq(
              q"""implicit class $defname[..$tparams](x: Definition[$tpname[..$argTParams]]) { ..$extensions }""",
            ),
            tpname
          )

      }
      val newObj = objOpt match {
        case None => q"""object ${tpname.toTermName} { ..$implicitClzs } """
        case _ =>
          throw new Exception(
            s"TODO"
          )
      }

      println("class:")
      println(clz)
      println("new Object:")
      println(newObj)

      q"""
        $clz

        $newObj
      """
      // annottees.map(_.tree).toList
    }

    // c.Expr[Any](Block(result, Literal(Constant(()))))
    c.Expr[Any](result)
  }
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class have_a_try extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro tryMacro.impl
}

class public extends StaticAnnotation
