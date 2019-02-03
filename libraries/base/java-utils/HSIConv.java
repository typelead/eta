package eta.base;

import java.util.Arrays;
import java.io.UnsupportedEncodingException;
import java.nio.Buffer;
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

    public final CharsetDecoder decoder;
    public final CharsetEncoder encoder;

    public HSIConv(CharsetDecoder decoder, CharsetEncoder encoder) {
        this.decoder = decoder;
        this.encoder = encoder;
    }

    private static final boolean debug = false;

    private static void debug(String msg) {
        if (debug) {
            System.out.println("HSIConv [debug] " +
                               Thread.currentThread() + ":" + msg);
        }
    }

     private static void error(String msg) {
        if (debug) {
            System.err.println("HSIConv [error] " +
                               Thread.currentThread() + ":" + msg);
        }
    }
    
    public static HSIConv hs_iconv_open(String toEncodingStr, String fromEncodingStr) {
        debug("Opening iconv from " + fromEncodingStr + " to "
                + toEncodingStr);
        return new HSIConv(Charset.forName(fromEncodingStr).newDecoder(),
                           Charset.forName(toEncodingStr).newEncoder());
    }

    public static int hs_iconv_close(HSIConv iconv) {
        debug("Closing iconv with id: " + iconv);
        return 0;
    }

    private final static int E2BIG  = 7;
    private final static int EINVAL = 22;
    private final static int EILSEQ = 84;

    public static int hs_iconv(HSIConv iconv, long inbufptr, long inleftptr,
                               long outbufptr, long outleftptr) {
        int charsWritten = 0;
        try {
            int inleft = buffGetInt(inleftptr);
            int outleft = buffGetInt(outleftptr);
            debug("Recoding from: " + iconv.decoder.charset().displayName()
                         + ", to: " + iconv.encoder.charset().displayName());
            debug("in num bytes left: "+inleft);
            debug("out num bytes left: "+outleft);
            if (inbufptr != 0L && inleft != 0L) {
                debug("Init in buffer:");
                ByteBuffer inbuf     = initBuffer(inbufptr, inleftptr, true);
                int        inInitPos = inbuf.position();
                debug("Init out buffer:");
                ByteBuffer outbuf     = initBuffer(outbufptr, outleftptr, false);
                int        outInitPos = outbuf.position();
                charsWritten = recode(iconv, inbuf, outbuf);
                debug("After encoding:");
                debug("In buffer: "  + inbuf);
                debug("Out buffer: " + outbuf);
                int inFinalPos      = inbuf.position();
                int outFinalPos     = outbuf.position();
                int inBytesRead     = inFinalPos  - inInitPos;
                int outBytesWritten = outFinalPos - outInitPos;
                debug("Bytes read: "    + inBytesRead);
                debug("Bytes written: " + outBytesWritten);
                buffAddLong(inbufptr, inBytesRead);
                buffAddInt(inleftptr, -inBytesRead);
                buffAddLong(outbufptr, outBytesWritten);
                buffAddInt(outleftptr, -outBytesWritten);
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

    private static int buffGetInt(long bufAddress) {
        return MemoryManager.getInt(bufAddress);
    }
    
    private static void buffAddInt(long bufAddress, int toAdd) {
        MemoryManager.putInt(bufAddress,
                             MemoryManager.getInt(bufAddress) + toAdd);
    }

    private static void buffAddLong(long bufAddress, long toAdd) {
        MemoryManager.putLong(bufAddress,
                              MemoryManager.getLong(bufAddress) + toAdd);
    }

    private static ByteBuffer initBuffer(long bufptrAddress,
                                         long leftAddress, boolean input) {
        int limit      = MemoryManager.getInt(leftAddress);
        long memAddr   = MemoryManager.getLong(bufptrAddress);
        ByteBuffer buf = MemoryManager.getBoundedBuffer(memAddr);
        ((Buffer)buf).limit(buf.position() + limit);
        byte[] contents = new byte[limit];
        buf.duplicate().get(contents);
        debug("initBuffer: address = " + memAddr +
              ", limit = " + limit + ", buffer = " + buf);
        return buf;
    }

    private static int recode(HSIConv iconv, ByteBuffer inbuf,
                              ByteBuffer outbuf) {
        int charsWritten   = 0;
        CharsetDecoder dec = iconv.decoder;
        CharsetEncoder enc = iconv.encoder;
        try {
            
            if (inbuf.remaining() == 0 || outbuf.remaining() == 0)
                return 0;

            CharBuffer buf16 = CharBuffer.allocate(1);

            for (;;) {
                int inInitPos = inbuf.position();
                ((Buffer)buf16).clear();

                CoderResult decRes =  dec.decode(inbuf, buf16, true);
               
                if (decRes.isUnderflow())
                    decRes = dec.flush(buf16);
                else if (decRes.isError()) {
                    error("Error decoding: " + decRes); 
                    if (decRes.isMalformed() &&
                        decRes.length() == inbuf.remaining())
                        charsWritten = -EINVAL;
                    else
                        charsWritten = -EILSEQ;
                    break;
                }
                ((Buffer)buf16).flip();

                CoderResult encRes = enc.encode(buf16,outbuf,true);
                if (encRes.isUnderflow()) {
                    charsWritten ++;
                } else {
                    if (encRes.isOverflow())
                        charsWritten = -E2BIG;
                    else if (encRes.isError()) {
                        error("Error encoding: "+encRes);
                        charsWritten = -EILSEQ;
                    }
                    ((Buffer)inbuf).position(inInitPos);
                    break;
                }
                if (decRes.isUnderflow() || !inbuf.hasRemaining())
                    break;
               
            }
            debug("Chars written: " + charsWritten);
        } finally {
            dec.reset();
            enc.reset();
        }
        if (charsWritten == -1) {
            error(MemoryManager.getHeap().getStatistics().
                  generateReport());
        }
        return charsWritten;
    }
}
