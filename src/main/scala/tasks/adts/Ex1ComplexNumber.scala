package tasks.adts

package u04lab

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    private case class ComplNum(re: Double, im: Double)
    opaque type Complex = ComplNum
    def complex(re: Double, im: Double): Complex = ComplNum(re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case ComplNum(re, _) => re
      def im(): Double = complex match
        case ComplNum(_, im) => im
      def sum(other: Complex): Complex = (complex, other) match
        case (ComplNum(re1, im1), ComplNum(re2, im2)) => ComplNum(re1 + re2, im1 + im2)
      def subtract(other: Complex): Complex = (complex, other) match
        case (ComplNum(re1, im1), ComplNum(re2, im2)) => ComplNum(re1 - re2, im1 - im2)
      def asString(): String = complex match
        case ComplNum(re, 0) => s"$re"
        case ComplNum(0, im) => im + "i"
        case ComplNum(re, im) => if (im > 0) then s"$re + ${im}i" else s"$re - ${-im}i"
