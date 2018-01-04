package eta.base;

import java.util.Arrays;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.util.HashMap;
import java.util.Map;

import eta.base.Utils;
import eta.runtime.io.MemoryManager;
/**
 * Traduction of iconv gnu c lib (https://www.gnu.org/software/libiconv/) to java for foreign calls in GHC.IO.Encoding.Iconv eta module.
 * Using java.nio.charset classes (https://docs.oracle.com/javase/8/docs/api/java/nio/charset/Charset.html)
 *
 */
public class HSIConv {

    private static final ThreadLocal<Map<Long, String[]>> iconvs
        = new ThreadLocal<Map<Long, String[]>>();
    private static final boolean debug = false;

    private static void initIconvs() {
        if (iconvs.get() == null) {
            iconvs.set(new HashMap<Long, String[]>());
        }
    }

    private static void debug(String msg) {
        if (debug) {
            System.out.println(msg);
        }
    }

    public static long hs_iconv_open(String toEncodingStr, String fromEncodingStr) {
        initIconvs();
        debug("HSIConv: Opening iconv from " + fromEncodingStr + " to "
                + toEncodingStr);
        String[] fromTo = new String[] { fromEncodingStr, toEncodingStr };
        long id = (long) fromTo.hashCode();
        iconvs.get().put(id, fromTo);
        return id;
    }

    public static int hs_iconv_close(long iconv) {
        debug("HSIConv: Closing iconv with id: " + iconv);
        return 0;
    }

    private final static int E2BIG  = 7;
    private final static int EINVAL = 22;
    private final static int EILSEQ = 84;

    public static int hs_iconv(long iconv, long inbufptr, long inleft,
                               long outbufptr, long outleft) {
        int        charsWritten = 0;
        String[]   fromTo       = iconvs.get().get(iconv);
        try {
            debug("HSIConv: iconv with id: " + iconv + ", from: " + fromTo[0] +
                  ", to: " + fromTo[1]);
            if (inbufptr != 0L && inleft != 0L) {
                debug("Init in buffer:");
                ByteBuffer inbuf     = initBuffer(inbufptr, inleft, true);
                int        inInitPos = inbuf.position();
                debug("Init out buffer:");
                ByteBuffer outbuf     = initBuffer(outbufptr, outleft, false);
                int        outInitPos = outbuf.position();
                charsWritten = recode(fromTo, inbuf, outbuf);
                debug("After encoding:");
                debug("IN: buffer: "  + inbuf);
                debug("OUT: buffer: " + outbuf);
                int inFinalPos      = inbuf.position();
                int outFinalPos     = outbuf.position();
                int inBytesRead     = inFinalPos  - inInitPos;
                int outBytesWritten = outFinalPos - outInitPos;
                debug("Bytes read: "    + inBytesRead);
                debug("Bytes written: " + outBytesWritten);
                buffAddLong(inbufptr, inBytesRead);
                buffAddInt(inleft, -inBytesRead);
                buffAddLong(outbufptr, outBytesWritten);
                buffAddInt(outleft, -outBytesWritten);
            } else if (outbufptr != 0L && outleft != 0L) {
                debug("in is null");
                /** In this case, the iconv function attempts to set cd's conversion
                 *  state to the initial state and store a corresponding shift sequence
                 *  at *outbuf.
                 *  At most *outbytesleft bytes, starting at *outbuf, will be written.
                 */
            } else {
                debug("in and out is null");
                /** the iconv function sets cd's conversion state to the initial
                    state. */
            }
        } catch (Exception e) {
            debug("Error in recoding: " + e);
            if (debug) {
                e.printStackTrace();
            }
            throw new RuntimeException(e);
        }
        return charsWritten;
    }

    private static void buffAddInt(long bufAddress, int toAdd) {
        MemoryManager.putInt(bufAddress, MemoryManager.getInt(bufAddress) + toAdd);
    }

    private static void buffAddLong(long bufAddress, long toAdd) {
        MemoryManager.putLong(bufAddress, MemoryManager.getLong(bufAddress) + toAdd);
    }

    private static ByteBuffer initBuffer(long bufptrAddress, long leftAddress, boolean input) {
        int limit      = MemoryManager.getInt(leftAddress);
        long memAddr   = MemoryManager.getLong(bufptrAddress);
        ByteBuffer buf = MemoryManager.getBoundedBuffer(memAddr);
        buf.limit(buf.position() + limit);
        byte[] contents = new byte[limit];
        buf.duplicate().get(contents);
        debug("initBuffer: address: " + memAddr + ", limit: " + limit +
              ", buffer: " + buf + (input? ", contents: " + Arrays.toString(contents) : ""));
        return buf;
    }

    private static int recode(String[] fromTo, ByteBuffer inbuf, ByteBuffer outbuf) {
        /* Using always fresh coders: maybe it could cause performance penalty
           In that case we'll have to cache them (using hs_iconv_open and
           hs_iconv_close) and handle theirs states. */

        int charsWritten   = 0;
        CharsetDecoder dec = Charset.forName(fromTo[0]).newDecoder();
        CharsetEncoder enc = Charset.forName(fromTo[1]).newEncoder();
        try {
            /* No overflow in the intermediate out buffer using this method. */
            CharBuffer buf16 = dec.decode(inbuf);
            debug("String decoded: " + buf16.toString());
            /* iconv states "In this case iconv returns the number of non-reversible
               conversions performed during this call."
               We simply return the size of the intermediate buffer. */
            charsWritten = buf16.limit();
            CoderResult encRes = enc.encode(buf16, outbuf, true);
            debug("Encoding result: " + encRes);
            if (encRes.isUnderflow()) {
                enc.flush(outbuf);
            } else {
                charsWritten = -1;
                if (encRes.isOverflow()) {
                    Utils.set_errno(E2BIG);
                } else if(encRes.isError()) {
                    Utils.set_errno(EILSEQ);
                }
            }
        } catch (CharacterCodingException e) {
            debug("Error decoding string: " + e);
            if (debug) {
                e.printStackTrace();
            }
            charsWritten = -1;
            // TODO: handle EINVAL case
            Utils.set_errno(EILSEQ);
        }
        if (charsWritten == -1 && debug) {
            System.err.println(MemoryManager.getHeap().getStatistics().generateReport());
        }
        return charsWritten;
    }
}
