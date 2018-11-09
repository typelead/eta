#ifndef MACHDEPS_H
#define MACHDEPS_H

/* Sizes of Eta types follow.  These sizes correspond to:
 *   - the number of bytes in the primitive type (eg. Int#)
 *   - the number of bytes in the external representation (eg. HsInt)
 *   - the scale offset used by writeFooOffAddr#
 *
 * In the heap, the type may take up more space: eg. SIZEOF_INT8 == 1,
 * but it takes up SIZEOF_HSWORD (4 or 8) bytes in the heap.
 */

#define WORD_SIZE_IN_BITS        32
#define WORD_SIZE_IN_BITS_FLOAT  32.0

#define ALIGNMENT_CHAR           1
#define ALIGNMENT_DOUBLE         8
#define ALIGNMENT_FLOAT          4
#define ALIGNMENT_INT            4
#define ALIGNMENT_LONG           8
#define ALIGNMENT_SHORT          2
#define ALIGNMENT_UNSIGNED_CHAR  1
#define ALIGNMENT_UNSIGNED_INT   4
#define ALIGNMENT_UNSIGNED_LONG  8
#define ALIGNMENT_UNSIGNED_SHORT 2
#define ALIGNMENT_VOID_P         8
#define SIZEOF_CHAR              1
#define SIZEOF_DOUBLE            8
#define SIZEOF_FLOAT             4
#define SIZEOF_INT               4
#define SIZEOF_LONG              8
#define SIZEOF_SHORT             2
#define SIZEOF_UNSIGNED_CHAR     1
#define SIZEOF_UNSIGNED_INT      4
#define SIZEOF_UNSIGNED_LONG     8
#define SIZEOF_UNSIGNED_SHORT    2
#define SIZEOF_VOID_P            8

#define SIZEOF_INT8             SIZEOF_CHAR
#define ALIGNMENT_INT8          ALIGNMENT_CHAR

#define SIZEOF_WORD8            SIZEOF_UNSIGNED_CHAR
#define ALIGNMENT_WORD8         ALIGNMENT_UNSIGNED_CHAR

#define SIZEOF_INT16            SIZEOF_SHORT
#define ALIGNMENT_INT16         ALIGNMENT_SHORT

#define SIZEOF_WORD16           SIZEOF_UNSIGNED_SHORT
#define ALIGNMENT_WORD16        ALIGNMENT_UNSIGNED_SHORT

#define SIZEOF_INT32            SIZEOF_INT
#define ALIGNMENT_INT32         ALIGNMENT_INT

#define SIZEOF_WORD32           SIZEOF_UNSIGNED_INT
#define ALIGNMENT_WORD32        ALIGNMENT_UNSIGNED_INT

#define SIZEOF_INT64            SIZEOF_LONG
#define ALIGNMENT_INT64         ALIGNMENT_LONG

#define SIZEOF_WORD64           SIZEOF_UNSIGNED_LONG
#define ALIGNMENT_WORD64        ALIGNMENT_UNSIGNED_LONG

#define SIZEOF_HSCHAR           SIZEOF_WORD32
#define ALIGNMENT_HSCHAR        ALIGNMENT_WORD32

#define SIZEOF_HSINT            SIZEOF_INT
#define ALIGNMENT_HSINT         ALIGNMENT_INT

#define SIZEOF_HSWORD           SIZEOF_INT
#define ALIGNMENT_HSWORD        ALIGNMENT_INT

#define SIZEOF_HSDOUBLE         SIZEOF_DOUBLE
#define ALIGNMENT_HSDOUBLE      ALIGNMENT_DOUBLE

#define SIZEOF_HSFLOAT          SIZEOF_FLOAT
#define ALIGNMENT_HSFLOAT       ALIGNMENT_FLOAT

#define SIZEOF_HSPTR            SIZEOF_VOID_P
#define ALIGNMENT_HSPTR         ALIGNMENT_VOID_P

#define SIZEOF_HSFUNPTR         SIZEOF_VOID_P
#define ALIGNMENT_HSFUNPTR      ALIGNMENT_VOID_P

#define SIZEOF_HSSTABLEPTR      SIZEOF_INT
#define ALIGNMENT_HSSTABLEPTR   ALIGNMENT_INT

/* The JVM uses big-endian. */
#define WORDS_BIGENDIAN

#endif /* MACHDEPS_H */
