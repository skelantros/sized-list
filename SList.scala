import scala.compiletime.ops.int.*
import scala.compiletime.*

sealed trait SList[+A, N <: Int](using GeqZ[N]) {
    inline def size: N = constValue[N]

    def head(using GtZ[N]): A = this.asInstanceOf[SCons[A, N - 1]].head
    def tail(using GtZ[N]): SList[A, N - 1] = this.asInstanceOf[SCons[A, N - 1]].tail

    def ::[A1 >: A](x: A1): SList[A1, S[N]] = SCons(x, this)

    def :+[A1 >: A](x: A1): SList[A1, S[N]] = this match {
        case SCons(v, vs) => v :: (vs :+ x)
        case SNil => x :: SNil
    }

    def map[B](f: A => B): SList[B, N] = this match {
        case SCons(v, vs) => f(v) :: vs.map(f)
        case SNil => SNil
    }

    def foldLeft[B](zero: B)(f: (B, A) => B): B = this match {
        case SCons(v, vs) => vs.foldLeft(f(zero, v))(f)
        case SNil => zero
    }

    transparent inline def refined: SList[A, N] = inline constValueOpt[N] match {
        case Some(n) if n == 0 =>
            summonFrom {
                case given (SNil.type <:< SList[A, N]) => SNil
                case _ => error(s"Cannot prove that SNil <:< SList[A, ${constValue[N]}]")
            }
        case Some(_) =>
            summonFrom {
                case given (SCons[A, N - 1] <:< SList[A, N]) => this.asInstanceOf[SCons[A, N - 1]]
                case _ => error(s"Cannot prove that SCons[A, ${constValue[N] - 1}] <:< SList[A, ${constValue[N]}]")
            }
        case None => this
    }

    inline def :::[A1 >: A, N1 <: Int](begin: SList[A1, N1]): SList[A1, N1 + N] = SList.add(begin, this)
    inline def ++[A1 >: A, N1 <: Int](end: SList[A1, N1]): SList[A1, N + N1] = SList.add(this, end)

    inline def flatten[B, N1 <: Int](using ev: A <:< SList[B, N1]): SList[B, N * N1] = SList.flatten(this.map(ev))
    inline def flatMap[B, N1 <: Int](f: A => SList[B, N1]): SList[B, N * N1] = this.map(f).flatten

    def toList: List[A] = this match {
        case SCons(x, xs) => x :: xs.toList
        case SNil => Nil
    }

    override def toString: String = this.toList.mkString("SList(", ",", ")")
}

object SList {

    inline def add[A, N1 <: Int, N2 <: Int](list1: SList[A, N1], list2: SList[A, N2]): SList[A, N1 + N2] =
        inline constValueOpt[N1] match {
            case Some(n) if n == 0 =>
                summonFrom {
                    case given (SList[A, N2] =:= SList[A, N1 + N2]) => list2
                }
            case Some(_) =>
                val SCons(x, xs) = list1.asInstanceOf[SCons[A, N1 - 1]]
                summonFrom {
                    case given (SList[A, S[N1 - 1 + N2]] =:= SList[A, N1 + N2]) => x :: add(xs, list2)
                }
        }

    inline def addUsingRefined[A, N1 <: Int, N2 <: Int](list1: SList[A, N1], list2: SList[A, N2]): SList[A, N1 + N2] =
        inline list1.refined match {
            case cons: SCons[A, N1 - 1] =>
                summonFrom {
                    case given (SList[A, S[N1 - 1 + N2]] =:= SList[A, N1 + N2]) => cons.head :: add(cons.tail, list2)
                }
            case _: SNil.type =>
                summonFrom {
                    case given (SList[A, N2] =:= SList[A, N1 + N2]) => list2
                }
        }

    def addRuntime[A, N1 <: Int, N2 <: Int](list1: SList[A, N1], list2: SList[A, N2]): SList[A, N1 + N2] = {
        def addUnsized(list1: SList[A, ?], list2: SList[A, ?]): SList[A, ?] = list1 match {
            case SCons(x, xs) => x :: addUnsized(xs, list2)
            case SNil => list2
        }

        addUnsized(list1, list2).asInstanceOf[SList[A, N1 + N2]]
    }

    inline def flatten[A, N0 <: Int, N <: Int](list: SList[SList[A, N], N0]): SList[A, N0 * N] =
        inline list.refined match {
            case cons: SCons[SList[A, N], N0 - 1] =>
                summonFrom {
                    case given (SList[A, N + (N0 - 1) * N] =:= SList[A, N0 * N]) => cons.head ::: flatten(cons.tail)
                }
            case _: SNil.type =>
                summonFrom {
                    case given (SNil.type <:< SList[A, N0 * N]) => SNil
                }
        }
}

case object SNil extends SList[Nothing, 0]
case class SCons[+A, N <: Int](head: A, tail: SList[A, N])(using GeqZ[N]) extends SList[A, S[N]](using GtZ.snNext)
