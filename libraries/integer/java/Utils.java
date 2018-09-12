package eta.integer;

import java.math.BigInteger;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Closure;

public class Utils {
    /**
     * Taken from:
     * https://github.com/bcgit/bc-java/blob/master/core/src/main/java/org/bouncycastle/pqc/math/ntru/euclid/BigIntEuclidean.java
     */
    public static BigInteger[] extendedEuclid(BigInteger a, BigInteger b) {
        BigInteger x = BigInteger.ZERO;
        BigInteger lastx = BigInteger.ONE;
        BigInteger y = BigInteger.ONE;
        BigInteger lasty = BigInteger.ZERO;
        while (!b.equals(BigInteger.ZERO))
            {
                BigInteger[] quotientAndRemainder = a.divideAndRemainder(b);
                BigInteger quotient = quotientAndRemainder[0];

                BigInteger temp = a;
                a = b;
                b = quotientAndRemainder[1];

                temp = x;
                x = lastx.subtract(quotient.multiply(x));
                lastx = temp;

                temp = y;
                y = lasty.subtract(quotient.multiply(y));
                lasty = temp;
            }
        return new BigInteger[] {a, lastx};
    }

    public static int gcd(int p, int q) {
        while (q != 0) {
            int temp = q;
            q = p % q;
            p = temp;
        }
        return p;
    }

    /**
     * Return a BigInteger equal to the unsigned value of the
     * argument.
     */
    public static BigInteger toUnsignedBigInteger(long i) {
        if (i >= 0L)
            return BigInteger.valueOf(i);
        else {
            int upper = (int) (i >>> 32);
            int lower = (int) i;

            // return (upper << 32) + lower
            return (BigInteger.valueOf(toUnsignedLong(upper))).shiftLeft(32).
                add(BigInteger.valueOf(toUnsignedLong(lower)));
        }
    }

    public static BigInteger toUnsignedBigInteger(int i) {
      return Utils.toUnsignedBigInteger(toUnsignedLong(i));
    }

    /**
     * Converts the argument to a {@code long} by an unsigned
     * conversion.  In an unsigned conversion to a {@code long}, the
     * high-order 32 bits of the {@code long} are zero and the
     * low-order 32 bits are equal to the bits of the integer
     * argument.
     *
     * Consequently, zero and positive {@code int} values are mapped
     * to a numerically equal {@code long} value and negative {@code
     * int} values are mapped to a {@code long} value equal to the
     * input plus 2<sup>32</sup>.
     *
     * @param  x the value to convert to an unsigned {@code long}
     * @return the argument converted to {@code long} by an unsigned
     *         conversion
     * @since 1.8
     */
    public static long toUnsignedLong(int x) {
        return ((long) x) & 0xffffffffL;
    }

    /**
     * Returns the unsigned quotient of dividing the first argument by
     * the second where each argument and the result is interpreted as
     * an unsigned value.
     *
     * <p>Note that in two's complement arithmetic, the three other
     * basic arithmetic operations of add, subtract, and multiply are
     * bit-wise identical if the two operands are regarded as both
     * being signed or both being unsigned.  Therefore separate {@code
     * addUnsigned}, etc. methods are not provided.
     *
     * @param dividend the value to be divided
     * @param divisor the value doing the dividing
     * @return the unsigned quotient of the first argument divided by
     * the second argument
     * @see #remainderUnsigned
     * @since 1.8
     */
    public static long divideUnsigned(long dividend, long divisor) {
        if (divisor < 0L) { // signed comparison
            // Answer must be 0 or 1 depending on relative magnitude
            // of dividend and divisor.
            return (compareUnsigned(dividend, divisor)) < 0 ? 0L :1L;
        }

        if (dividend > 0) //  Both inputs non-negative
            return dividend/divisor;
        else {
            /*
             * For simple code, leveraging BigInteger.  Longer and faster
             * code written directly in terms of operations on longs is
             * possible; see "Hacker's Delight" for divide and remainder
             * algorithms.
             */
            return toUnsignedBigInteger(dividend).
                divide(toUnsignedBigInteger(divisor)).longValue();
        }
    }

    /**
     * Returns the unsigned remainder from dividing the first argument
     * by the second where each argument and the result is interpreted
     * as an unsigned value.
     *
     * @param dividend the value to be divided
     * @param divisor the value doing the dividing
     * @return the unsigned remainder of the first argument divided by
     * the second argument
     * @see #divideUnsigned
     * @since 1.8
     */
    public static long remainderUnsigned(long dividend, long divisor) {
        if (dividend > 0 && divisor > 0) { // signed comparisons
            return dividend % divisor;
        } else {
            if (compareUnsigned(dividend, divisor) < 0) // Avoid explicit check for 0 divisor
                return dividend;
            else
                return toUnsignedBigInteger(dividend).
                    remainder(toUnsignedBigInteger(divisor)).longValue();
        }
    }

    /**
     * Compares two {@code long} values numerically treating the values
     * as unsigned.
     *
     * @param  x the first {@code long} to compare
     * @param  y the second {@code long} to compare
     * @return the value {@code 0} if {@code x == y}; a value less
     *         than {@code 0} if {@code x < y} as unsigned values; and
     *         a value greater than {@code 0} if {@code x > y} as
     *         unsigned values
     * @since 1.8
     */
    public static int compareUnsigned(long x, long y) {
        return compareLong(x + Long.MIN_VALUE, y + Long.MIN_VALUE);
    }

    /**
     * Compares two {@code long} values numerically.
     * The value returned is identical to what would be returned by:
     * <pre>
     *    Long.valueOf(x).compareTo(Long.valueOf(y))
     * </pre>
     *
     * @param  x the first {@code long} to compare
     * @param  y the second {@code long} to compare
     * @return the value {@code 0} if {@code x == y};
     *         a value less than {@code 0} if {@code x < y}; and
     *         a value greater than {@code 0} if {@code x > y}
     * @since 1.7
     */
    public static int compareLong(long x, long y) {
        return (x < y) ? -1 : ((x == y) ? 0 : 1);
    }

    /* Encoding & Decoding Floats/Doubles */
    // TODO: 1) Do we need to treat 0.0 case as special?
    //       2) Math.scalb is not present in Java 1.5.
    //       3) Do we need to treat negative case separately?
    public static float int_encodeFloat(int j, int e) {
        return Math.scalb((float) j, e);
    }

    public static double int_encodeDouble(int j, int e) {
        return Math.scalb((double) j, e);
    }

    public static float encodeFloat(BigInteger j, int e) {
        return Math.scalb(j.floatValue(), e);
    }

    public static double encodeDouble(BigInteger j, int e) {
        return Math.scalb(j.doubleValue(), e);
    }

    // TODO: Optimize this - maybe make it an inlined primop?
    public static Closure decodeDouble(StgContext context, double d) {
        long bits = Double.doubleToRawLongBits(d);
        int s = ((bits >> 63) == 0) ? 1 : -1;
        int e = (int)((bits >> 52) & 0x7ffL);
        long m = (e == 0) ?
            (bits & 0xfffffffffffffL) << 1 :
            (bits & 0xfffffffffffffL) | 0x10000000000000L;
        context.I1 = e - 1075;
        context.O1 = BigInteger.valueOf(s * m);
        return null;
    }

    /* Taken from Google Guava BigIntMath */
    public static int log2(BigInteger x) {
        return x.bitLength() - 1;
    }

    public static int log2(int x) {
        return (Integer.SIZE - 1) - Integer.numberOfLeadingZeros(x);
    }

    public static boolean isPowerOfTwo(BigInteger x) {
        return !x.and(x.subtract(BigInteger.ONE)).equals(BigInteger.ZERO);
    }

    /**
     * Returns {@code true} if {@code x} represents a power of two.
     *
     * <p>This differs from {@code Integer.bitCount(x) == 1}, because
     * {@code Integer.bitCount(Integer.MIN_VALUE) == 1}, but {@link Integer#MIN_VALUE} is not a power
     * of two.
     */
     public static boolean isPowerOfTwo(int x) {
         return ((x & (x - 1)) != 0);
     }
}
