import scala.compiletime.ops.int.*

/**
 * A proof instance that N >= 0
 */
sealed trait GeqZ[N <: Int]
object GeqZ {
    given[N <: Int](using N >= 0 =:= true): GeqZ[N] = new GeqZ[N] {}
}

/**
 * A proof instance that N > 0
 */
sealed trait GtZ[N <: Int] extends GeqZ[N]
object GtZ {
    given[N <: Int](using N > 0 =:= true): GtZ[N] = new GtZ[N] {}

    /**
     * An explicit Peano-like proof that S[N] > 0 based on N >= 0
     */
    def snNext[N <: Int](using GeqZ[N]): GtZ[S[N]] = new GtZ[S[N]] {}
}