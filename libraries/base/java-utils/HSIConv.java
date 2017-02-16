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
	
	private static final ThreadLocal<Map<Long,String[]>> iconvs=new ThreadLocal<>();
	private static final boolean debug=true;
	
	private static void initIconvs() {
		if (iconvs.get()==null)
			iconvs.set(new HashMap<>());
	}
	
	private static Boolean isDebugEnabled() {
		return debug;
	}
	
	private static void debug(String msg) {
		if (isDebugEnabled())
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
			debug("Error opening iconv: "+e);
			if (isDebugEnabled())
				e.printStackTrace();
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
		int charsWritten=0; 
		try {
			debug("HSIConv: iconv with id: "+iconv+", from: "+fromTo[0]+", to: "+fromTo[1]);
			if (inbufptr!=null && inleft!=null) {
				debug("Init in buffer:");
				ByteBuffer inbuf=initBuffer(inbufptr, inleft);
				int inInitPos=inbuf.position();
	
				debug("Init out buffer:");
				ByteBuffer outbuf=initBuffer(outbufptr,outleft);
				int outInitPos=outbuf.position();
				
				charsWritten=recode(fromTo,inbuf,outbuf);

				debug("After encoding:");
				debug("IN: buffer: "+inbuf);
				debug("OUT: buffer: "+outbuf);
				int inFinalPos=inbuf.position();
				int outFinalPos=outbuf.position();
				int inBytesRead=inFinalPos-inInitPos;
				int outBytesWritten=outFinalPos-outInitPos;
				debug("Bytes read: "+inBytesRead);
				debug("Bytes written: "+outBytesWritten);
				
				buffAddInt(inbufptr,0,inBytesRead);
				buffAddInt(inleft,0,-inBytesRead);
				
				buffAddInt(outbufptr,0,outBytesWritten);
				buffAddInt(outleft,0,-outBytesWritten);
				
				
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
			debug("Error in recoding: "+e);
			if (isDebugEnabled())
					e.printStackTrace();
			throw new RuntimeException(e);
		}
		return charsWritten;
	}

	private static void buffAddInt(ByteBuffer buf, int pos, int toAdd) {
		int prev=MemoryManager.bufGetInt(buf,pos);
		MemoryManager.bufPutInt(buf, pos, prev+toAdd);
	}

	private static ByteBuffer initBuffer(ByteBuffer bufptr, ByteBuffer left) {
		int memAddr=MemoryManager.bufGetInt(bufptr,0);
		ByteBuffer buf=MemoryManager.getBuffer(memAddr);
		int limit=MemoryManager.bufGetInt(left,0);
		buf=MemoryManager.bufSetLimit(buf, limit);
		debug("initBuffer: address: "+memAddr+", limit: "+limit+", buffer: "+buf);
		return buf;
	}

	private static int recode(String[] fromTo, ByteBuffer inbuf, ByteBuffer outbuf) {
		// Using always fresh coders: maybe it could cause performance penalty
		// In that case we'll have to cache them (using hs_iconv_open and hs_iconv_close) and handle theirs states 
		
		CharsetDecoder dec=Charset.forName(fromTo[0]).newDecoder();
		CharsetEncoder enc=Charset.forName(fromTo[1]).newEncoder();
		int charsWritten=0;
		try {
			// No overflow in the intermediate out buffer using this method
			CharBuffer buf16=dec.decode(inbuf);
			debug("String decoded: "+buf16.toString());
			// iconv states "In this case iconv returns the number of non-reversible conversions performed during this call."
			// we simply return the size of the intermediate buffer
			charsWritten=buf16.limit();
			CoderResult encRes=enc.encode(buf16, outbuf, true);
			debug("Encoding result: "+encRes);
			if (encRes.isUnderflow())
				enc.flush(outbuf);
			else  {
				charsWritten=-1;
				if (encRes.isOverflow())
					Utils.set_errno(E2BIG);
				else if(encRes.isError())
					Utils.set_errno(EILSEQ);
			}
		} catch (CharacterCodingException e) {
			debug("Error decoding string: "+e);
			if (isDebugEnabled())
				e.printStackTrace();
			charsWritten=-1;
			// TODO: handle EINVAL case
			Utils.set_errno(EILSEQ);
		}
		return charsWritten;
	}
	
}
