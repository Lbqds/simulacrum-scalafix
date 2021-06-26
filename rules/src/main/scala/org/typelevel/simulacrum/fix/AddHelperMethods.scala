package org.typelevel.simulacrum.fix

import scalafix.v1._
import scala.meta._

class AddHelperMethods extends SyntacticRule("AddHelperMethods") {
  import AddHelperMethods._

  override def description: String = "add helper methods for sealed trait"
  override def isLinter: Boolean = false

  private def isSealed(mods: List[Mod]): Boolean = mods.collectFirst {
    case res: Mod.Sealed => res
  }.isDefined

  private def genMethods(body: List[Tree], base: Type.Name): List[Defn] = {
    object Extractor extends ExtendFrom(base)
    body collect {
      case Defn.Class(_, name, tparams, ctor, Extractor(init)) =>
        val argss = ctor.paramss.map(_.map(param => Term.Name(param.name.value)))
        q"def ${Term.Name(name.value.toLowerCase)}[..$tparams](...${ctor.paramss}): ${init.tpe} = ${Term.Name(name.value)}(...$argss)"

      case Defn.Object(_, name, Extractor(init)) =>
        q"val ${Pat.Var(Term.Name(name.value.toLowerCase))}: ${init.tpe} = $name"
    }
  }

  private def find(source: Tree): Either[List[Diagnostic], List[Tree]] = source match {
    case Source(List(PackageAndBody(_, body))) =>
      val baseTraits = body.collect {
        case defn @ Defn.Trait(AddHelperMethodsAnnotation(_), _, _, _, _) => defn
      }
      val trees = baseTraits collect {
        case base @ Defn.Trait(mods, _, _, _, _) if isSealed(mods) =>
          val methods = genMethods(body, base.name)
          q"""object ${Term.Name(base.name.value)} {
                ..$methods
              }
           """
      }
      Right(trees)

    case _ => Right(Nil)
  }

  override def fix(implicit doc: SyntacticDocument): Patch = {
    find(doc.tree) match {
      case Left(errors) => Patch.fromIterable(errors.map(Patch.lint))
      case Right(trees) => Patch.addRight(doc.tree, trees.mkString("\n", "\n\n", "\n"))
    }
  }
}

object AddHelperMethods {
  class ExtendFrom(base: Type.Name) {
    def unapply(template: Template): Option[Init] = template.inits.collectFirst {
      case t @ Init(Type.Name(name), _, _) if name == base.value => t
      case t @ Init(Type.Apply(Type.Name(name), _), _, _) if name == base.value => t
    }
  }

  object AddHelperMethodsAnnotation {
    def isAnnotationNamed(name: String)(typeTree: Type): Boolean = typeTree match {
      case Type.Select(_, Type.Name(`name`)) => true
      case Type.Name(`name`)                 => true
      case _                                 => false
    }

    def unapply(mods: List[Mod]): Option[List[Term]] = mods.reverse.collectFirst {
      case Mod.Annot(Init(typeTree, Name(""), args)) if isAnnotationNamed("addHelperMethods")(typeTree) => Some(args.flatten)
    }.flatten
  }

  object PackageAndBody {
    private object PackageParts {
      def unapply(tree: Tree): Option[List[String]] =
        tree match {
          case Term.Select(base, Term.Name(last)) => unapply(base).map(last :: _)
          case Term.Name(p)                       => Some(List(p))
          case _                                  => None
        }
    }

    def unapply(tree: Tree): Option[(List[String], List[Tree])] = tree match {
      case Pkg(Term.Name(name), List(pkg @ Pkg(_, _))) =>
        unapply(pkg).map { case (inner, body) =>
          (name :: inner, body)
        }
      case Pkg(PackageParts(pkg), body) => Some(pkg.reverse, body)
      case _                            => None
    }
  }

  final case class InvalidAnnottee(position: Position) extends Diagnostic {
    override def message: String = "annottee must be sealed trait"
  }
}
