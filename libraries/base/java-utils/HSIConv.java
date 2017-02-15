package eta.base;
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
	
	private static ThreadLocal<Map<Long,String[]>> iconvs=new ThreadLocal<>();
	private static boolean debug=false;
	
	private static void initIconvs() {
		if (iconvs.get()==null)
			iconvs.set(new HashMap<>());
	}
	
	private static void debug(String msg) {
		if (debug)
			System.out.println(msg);
	}
	
	public static long hs_iconv_open(ByteBuffer toEncoding, ByteBuffer fromEncoding) {
		Long id=-1l;
		try {
			initIconvs();
			String fromEncodingStr=Utils.byteBufferToStr(fromEncoding);
			fromEncodingStr=fromEncodingStr.substring(0, fromEncodingStr.length()-1);
			String toEncodingStr=Utils.byteBufferToStr(toEncoding);
			toEncodingStr=toEncodingStr.substring(0, toEncodingStr.length()-1);
			debug("HSIConv: Opening iconv from "+fromEncodingStr+
					" to "+ toEncodingStr);
			String[] fromTo=new String[] {fromEncodingStr,toEncodingStr};
			id=(long)fromTo.hashCode();
			iconvs.get().put(id, fromTo);
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
		return id;
	}
	
	public static int hs_iconv_close(long iconv) {
		debug("HSIConv: Closing iconv with id: "+iconv);
		return 0;
	}
	private final static int E2BIG=7;
	private final static int EINVAL=22;
	private final static int EILSEQ=84;
	
	public static int hs_iconv(long iconv, ByteBuffer inbufptr, ByteBuffer inleft, 
			ByteBuffer outbufptr, ByteBuffer outleft) {
		String[] fromTo=iconvs.get().get(iconv);
		int charsWritten=-1; // error by default
		try {
			debug("HSIConv: iconv with id: "+iconv+", from: "+fromTo[0]+", to: "+fromTo[1]);
			if (inbufptr!=null && inleft!=null) {
				int inMemAddr=MemoryManager.bufGetInt(inbufptr,0);
				ByteBuffer inbuf=MemoryManager.getBuffer(inMemAddr);
				int inLimit=MemoryManager.bufGetInt(inleft,0);
				inbuf=MemoryManager.bufSetLimit(inbuf, inLimit);
				int inInitPos=inbuf.position();
				debug("IN: address: "+inMemAddr+", limit: "+inLimit+", buffer: "+inbuf);
	
				int outMemAddr=MemoryManager.bufGetInt(outbufptr,0);
				ByteBuffer outbuf=MemoryManager.getBuffer(outMemAddr);
				int outLimit=MemoryManager.bufGetInt(outleft,0);
				outbuf=MemoryManager.bufSetLimit(outbuf, outLimit);
				int outInitPos=outbuf.position();
				debug("OUT: address: "+outMemAddr+", limit: "+outLimit+", buffer: "+outbuf);
				// Using always fresh coders: maybe it could cause performance penalty
				// In that case we'll have to cache them (using hs_iconv_open and hs_iconv_close) and handle theirs states 
				CharsetDecoder dec=Charset.forName(fromTo[0]).newDecoder();
				CharsetEncoder enc=Charset.forName(fromTo[1]).newEncoder();
				try {
					// No overflow in the intermediate out buffer using this method
					CharBuffer buf16=dec.decode(inbuf);
					debug("String decoded: "+buf16.toString());
					// iconv states "In this case iconv returns the number of non-reversible conversions performed during this call."
					// we simply return the size of the intermediate buffer
					charsWritten=buf16.limit();
					CoderResult encRes=enc.encode(buf16, outbuf, true);
					debug("Encoding result: "+encRes);
					// The input in the intermediate buffer can't be malformed 
					if (encRes.isUnderflow())
						enc.flush(outbuf);
					else if (encRes.isOverflow())
						Utils.set_errno(E2BIG);
					else if(encRes.isUnmappable())
						Utils.set_errno(EILSEQ);
				} catch (CharacterCodingException e) {
					// TODO: handle EINVAL case
					Utils.set_errno(EILSEQ);
				}
				debug("IN: buffer: "+inbuf);
				debug("OUT: buffer: "+outbuf);
				int inFinalPos=inbuf.position();
				int outFinalPos=outbuf.position();
				int inBytesReaded=inFinalPos-inInitPos;
				int outBytesWritten=outFinalPos-outInitPos;
				
				MemoryManager.bufPutInt(inbufptr, 0, inMemAddr+inBytesReaded);
				MemoryManager.bufPutInt(inleft, 0, inLimit-inBytesReaded);
				
				MemoryManager.bufPutInt(outbufptr, 0, outMemAddr+outBytesWritten);
				MemoryManager.bufPutInt(outleft, 0, outLimit-outBytesWritten);
				
				
			} else if (outbufptr != null && outleft != null) {
				debug("in is null");
				/** In this case, the iconv function attempts to set cd’s conversion state
				 *  to the initial state and store a corresponding shift sequence at *outbuf. 
				 *  At most *outbytesleft bytes, starting at *outbuf, will be written.
				 */
			} else {
				debug("in and out is null");
				/** the iconv function sets cd’s conversion state to the initial state. */
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return charsWritten;
	}
	
}
