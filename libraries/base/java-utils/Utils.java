package eta.base;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.List;
import java.util.HashSet;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import java.nio.ByteOrder;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.OpenOption;
import java.nio.file.FileSystems;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.StandardOpenOption;
import java.nio.charset.Charset;
import java.nio.channels.Channels;
import java.nio.channels.Channel;
import java.nio.channels.FileChannel;
import java.nio.channels.Selector;
import java.nio.channels.SelectionKey;
import java.nio.channels.SelectableChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import eta.runtime.Runtime;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Capability;
import eta.runtime.stg.DataCon;
import eta.runtime.io.MemoryManager;
import eta.runtime.thunk.Thunk;
import eta.runtime.apply.PAP;
import eta.runtime.apply.Function;
import eta.runtime.RuntimeLogging;
import static eta.runtime.RuntimeLogging.*;

import ghc_prim.ghc.types.datacons.Czh;
import ghc_prim.ghc.types.datacons.ZC;
import ghc_prim.ghc.types.tycons.ZMZN;
import ghc_prim.ghc.Types;

import java.lang.management.ManagementFactory;
import com.sun.management.OperatingSystemMXBean;

public class Utils {
    // TODO: Verify correctness
    public static float rintFloat(float f) {
        return (float) Math.rint((double) f);
    }

    public static boolean iswprint(int codepoint) {
        switch (Character.getType(codepoint)) {
          case Character.UPPERCASE_LETTER:
          case Character.LOWERCASE_LETTER:
          case Character.TITLECASE_LETTER:
          case Character.MODIFIER_LETTER:
          case Character.OTHER_LETTER:
          case Character.COMBINING_SPACING_MARK:
          case Character.OTHER_NUMBER:
          case Character.MODIFIER_SYMBOL:
          case Character.ENCLOSING_MARK:
          case Character.DECIMAL_DIGIT_NUMBER:
          case Character.DASH_PUNCTUATION:
          case Character.OTHER_PUNCTUATION:
          case Character.CONNECTOR_PUNCTUATION:
          case Character.MATH_SYMBOL:
          case Character.SPACE_SEPARATOR:
          case Character.OTHER_SYMBOL:
          case Character.END_PUNCTUATION:
          case Character.FINAL_QUOTE_PUNCTUATION:
          case Character.START_PUNCTUATION:
          case Character.CURRENCY_SYMBOL:
          case Character.INITIAL_QUOTE_PUNCTUATION:
          case Character.LETTER_NUMBER:
          case Character.NON_SPACING_MARK:
            return true;
        }
        return false;
    }

    public static int wgencat(int codepoint) {
        switch (Character.getType(codepoint)) {
            case Character.UPPERCASE_LETTER:
                return 0;
            case Character.LOWERCASE_LETTER:
                return 1;
            case Character.TITLECASE_LETTER:
                return 2;
            case Character.MODIFIER_LETTER:
                return 3;
            case Character.OTHER_LETTER:
                return 4;
            case Character.NON_SPACING_MARK:
                return 5;
            case Character.COMBINING_SPACING_MARK:
                return 6;
            case Character.ENCLOSING_MARK:
                return 7;
            case Character.DECIMAL_DIGIT_NUMBER:
                return 8;
            case Character.LETTER_NUMBER:
                return 9;
            case Character.OTHER_NUMBER:
                return 10;
            case Character.CONNECTOR_PUNCTUATION:
                return 11;
            case Character.DASH_PUNCTUATION:
                return 12;
            case Character.START_PUNCTUATION:
                return 13;
            case Character.END_PUNCTUATION:
                return 14;
            case Character.INITIAL_QUOTE_PUNCTUATION:
                return 15;
            case Character.FINAL_QUOTE_PUNCTUATION:
                return 16;
            case Character.OTHER_PUNCTUATION:
                return 17;
            case Character.MATH_SYMBOL:
                return 18;
            case Character.CURRENCY_SYMBOL:
                return 19;
            case Character.MODIFIER_SYMBOL:
                return 20;
            case Character.OTHER_SYMBOL:
                return 21;
            case Character.SPACE_SEPARATOR:
                return 22;
            case Character.LINE_SEPARATOR:
                return 23;
            case Character.PARAGRAPH_SEPARATOR:
                return 24;
            case Character.CONTROL:
                return 25;
            case Character.FORMAT:
                return 26;
            case Character.SURROGATE:
                return 27;
            case Character.PRIVATE_USE:
                return 28;
            case Character.UNASSIGNED:
                return 29;
        }
        return -1;
    }

    public static boolean iswalnum(int codepoint) {
        switch (Character.getType(codepoint)) {
            case Character.UPPERCASE_LETTER:
            case Character.LOWERCASE_LETTER:
            case Character.TITLECASE_LETTER:
            case Character.MODIFIER_LETTER:
            case Character.OTHER_LETTER:
            case Character.DECIMAL_DIGIT_NUMBER:
            case Character.LETTER_NUMBER:
            case Character.OTHER_NUMBER:
                return true;
        }
        return false;
    }

    public static boolean iswspace(int codepoint) {
        return Character.getType(codepoint) == Character.SPACE_SEPARATOR;
    }

    public static boolean iswlower(int codepoint) {
        return Character.getType(codepoint) == Character.LOWERCASE_LETTER;
    }

    public static boolean iswupper(int codepoint) {
        switch (Character.getType(codepoint)) {
          case Character.UPPERCASE_LETTER:
          case Character.TITLECASE_LETTER:
              return true;
        }
        return false;
    }

    public static boolean isFloatNegativeZero(float f) {
        return Float.floatToRawIntBits(f) == 0x80000000;
    }

    public static boolean isFloatDenormalized(float f) {
        int bits = Float.floatToRawIntBits(f);
        return ((bits >> 23) & 0xff) == 0 && (bits & 0x7fffff) != 0;
    }

    public static boolean isFloatFinite(float f) {
        int bits = Float.floatToRawIntBits(f);
        return ((bits >> 23) & 0xff) != 0xff;
    }

    public static boolean isDoubleNegativeZero(double d) {
        return Double.doubleToRawLongBits(d) == 0x8000000000000000L;
    }

    public static boolean isDoubleDenormalized(double d) {
        long bits = Double.doubleToRawLongBits(d);
        return ((bits >> 52) & 0x7ffL) == 0 && (bits & 0xfffffffffffffL) != 0;
    }

    public static boolean isDoubleFinite(double d) {
        long bits = Double.doubleToRawLongBits(d);
        return ((bits >> 52) & 0x7ffL) != 0x7ffL;
    }

    public static int c_isatty(int tty) {
        return System.console() != null ? 1 : 0;
    }

    public static String c_localeEncoding() {
        return Charset.defaultCharset().name();
    }

    public static boolean isNonBlocking(Channel c) {
        if (c instanceof SelectableChannel) {
            return !((SelectableChannel) c).isBlocking();
        }
        return false;
    }

    public static int c_write(final Channel fd, final long address, final int count)
        throws IOException {
        // Clear interrupt status to avoid unnecessarily closing the stream.
        Thread.interrupted();
        if (Runtime.debugIO()) {
            debugIO("c_write: " + fd + " Address: " + address + " Count: " + count);
        }
        final boolean nonBlocking = isNonBlocking(fd);
        final WritableByteChannel wc = (WritableByteChannel) fd;
        final ByteBuffer buffer = MemoryManager.getBoundedBuffer(address);
        ((Buffer)buffer).limit(buffer.position() + count);
        final int written = wc.write(buffer);
        if (Runtime.debugIO()) {
            debugIO("c_write: " + fd + " return: " + written);
        }
        return written;
    }

    public static int c_read(final Channel fd, final long address, final int count)
        throws IOException {
        // Clear interrupt status to avoid unnecessarily closing the stream.
        Thread.interrupted();
        final boolean nonBlocking = isNonBlocking(fd);
        if (Runtime.debugIO()) {
            debugIO("c_read: " + fd.toString() + " Address: " + address + " Count: " + count +
                    " NonBlocking: " + nonBlocking);
        }
        final ReadableByteChannel rc = (ReadableByteChannel) fd;
        final ByteBuffer buffer = MemoryManager.getBoundedBuffer(address);
        int position = buffer.position();
        ((Buffer)buffer).limit(position + count);
        int size = rc.read(buffer);
        if (size == 0 && nonBlocking) {
            size = -1;
        } else if (size == -1) {
            size = 0;
        }
        if (Runtime.debugIO()) {
            debugIO("c_read: " + fd.toString() + " nonBlocking: " + nonBlocking
                    + " return: " + size);
        }
        if (Runtime.debugIOVerbose() && size > 0) {
            byte[] input = new byte[size];
            ((Buffer)buffer).position(position);
            buffer.get(input);
            debugIO("c_read: " + Arrays.toString(input));
        }
        return size;
    }

    public static String byteBufferToStr(long address, int len)
        throws UnsupportedEncodingException {
        return new String(eta.ghc_prim.Utils.byteBufferToBytes(address, len), "UTF-8");
    }

    public static String getOS() {
        final String originalRawName = System.getProperty("os.name");
        final String rawName = originalRawName.toLowerCase();
        if (rawName.startsWith("windows")) {
            return "mingw32";
        } else if (rawName.startsWith("mac")) {
            return "darwin";
        } else if (rawName.startsWith("linux")) {
            return "linux";
        } else if (rawName.startsWith("freebsd")) {
            return "freebsd";
        } else if (rawName.startsWith("sunos")) {
            return "solaris";
        } else {
            return originalRawName;
        }
    }

    public static String getArch() {
        return System.getProperty("os.arch");
    }

    public static boolean isBigEndian() {
        return ByteOrder.nativeOrder().equals(ByteOrder.BIG_ENDIAN);
    }

    public static boolean isNewlineCRLF() {
        return "\r\n".equals(System.lineSeparator());
    }

    public static void debugBelch(String format, String string) {
        RuntimeLogging.debugBelch(format, string);
    }

    public static void errorBelch(String format, String string) {
        RuntimeLogging.errorBelch(format, string);
    }

    private static boolean hasMXBean = false;

    static {
        try {
            Class.forName("java.lang.management.ManagementFactory");
            Class.forName("com.sun.management.OperatingSystemMXBean");
            hasMXBean = true;
        } catch (ClassNotFoundException cne) {}
    }

    /* Returns CPU time in picoseconds */
    public static long getCPUTime() {
        if (hasMXBean) {
            return ((OperatingSystemMXBean)
                    ManagementFactory.getOperatingSystemMXBean())
                .getProcessCpuTime()
                * 1000;
        } else {
            return (System.nanoTime() - Capability.startTimeNanos) * 1000;
        }
    }

    public static Channel getStdOut() {
        return Channels.newChannel(System.out);
    }

    public static Channel getStdIn() {
        return Channels.newChannel(System.in);
    }

    public static Channel getStdErr() {
        return Channels.newChannel(System.err);
    }

    private static ThreadLocal<Integer> errno = new ThreadLocal<Integer>();

    public static void initErrno() {
        if (errno.get() == null)
            errno.set(0);
    }

    public static int get_errno() {
        initErrno();
        return errno.get();
    }

    public static void set_errno(int errnoCode) {
        errno.set(errnoCode);
    }

    // Taken from: http://stackoverflow.com/questions/14524751/cast-object-to-generic-type-for-returning
    public static <T> T convertInstanceOfObject(Object o, Class<T> clazz) {
        try {
            return clazz.cast(o);
        } catch(ClassCastException e) {
            return null;
        }
    }

    public static boolean instanceOf(Object o, Class clazz) {
        return clazz.isInstance(o);
    }

    public static MessageDigest c_MD5Init() throws NoSuchAlgorithmException {
        return MessageDigest.getInstance("MD5");
    }

    public static void c_MD5Update(MessageDigest md, long address, int len) {
        ByteBuffer contents = MemoryManager.getBoundedBuffer(address);
        ((Buffer)contents).limit(contents.position() + len);
        md.update(contents);
    }

    public static void c_MD5Final(long address, MessageDigest md) {
        ByteBuffer result = MemoryManager.getBoundedBuffer(address);
        byte[]     hash   = md.digest();
        result.put(hash);
    }

    public static long _malloc(int size) {
        return MemoryManager.allocateBuffer(size, true);
    }

    public static long _calloc(int size, int bytes) {
        int        totalBytes = size * bytes;
        long       address    = MemoryManager.allocateBuffer(totalBytes, true);
        ByteBuffer buffer     = MemoryManager.getBoundedBuffer(address);
        int        times      = totalBytes / 8;
        int        remTimes   = totalBytes % 8;
        while (times-- != 0) {
            buffer.putLong(0);
        }
        while (remTimes-- != 0) {
            buffer.put((byte) 0);
        }
        return address;
    }

    public static long _realloc(long oldAddress, int newSize) {
        long newAddress = MemoryManager.allocateBuffer(newSize, true);
        int  oldSize    = MemoryManager.allocatedSize(oldAddress);
        c_memcpy(newAddress, oldAddress, Math.min(oldSize, newSize));
        MemoryManager.free(oldAddress);
        return newAddress;
    }

    public static long c_memcpy(long destAddress, long srcAddress, int size) {
        MemoryManager.copy(srcAddress,destAddress,size);
        return destAddress;
    }

    public static long c_memset(long address, int c_, int size) {
        MemoryManager.set(address, c_, size);
        return address;
    }

    public static long c_memmove(long destAddress, long srcAddress, int size) {
        MemoryManager.move(srcAddress, destAddress, size);
        return destAddress;
    }

    public static BasicFileAttributes c_fstat(Path p) throws IOException {
        return Files.readAttributes(p, BasicFileAttributes.class);
    }

    public static Object fileKey(final BasicFileAttributes attrs, final Path p) {
        Object key = attrs.fileKey();
        if (key == null) {
            /* Windows doesn't have fileKey, so we do the best we can. */
            try {
                key = p.toRealPath().normalize();
            } catch (IOException io) {
                key = p.toAbsolutePath().normalize();
            }
        }
        return key;
    }

    public static final Map<Object, Integer> fileLocks = new HashMap<Object, Integer>();

    public static  boolean lockFile(final Object key, final boolean forWriting) {
        synchronized (fileLocks) {
            final Integer readers = fileLocks.get(key);
            if (readers == null) {
                final int readersInt = forWriting? -1 : 1;
                fileLocks.put(key, readersInt);
            } else {
                if (forWriting || readers < 0) return false;
                fileLocks.put(key, readers + 1);
            }
            return true;
        }
    }

    public static boolean unlockFile(final Object key) {
        synchronized (fileLocks) {
            final Integer readers = fileLocks.get(key);
            if (readers == null) return false;
            int newReaders = 0;
            if (readers < 0) {
                newReaders = readers + 1;
            } else {
                newReaders = readers - 1;
            }
            if (newReaders == 0) {
                fileLocks.remove(key);
            } else {
                fileLocks.put(key, newReaders);
            }
            return true;
        }
    }

    public static void setNonBlockingFD(Channel c, boolean nonblocking) throws IOException {
        if (c instanceof SelectableChannel) {
            // Clear interrupt status to avoid unnecessarily closing the stream.
            Thread.interrupted();
            ((SelectableChannel) c).configureBlocking(!nonblocking);
        }
    }

    public static long c_lseek(FileChannel fc, long offset, int mode) throws IOException {
        // Clear interrupt status to avoid unnecessarily closing the stream.
        Thread.interrupted();
        switch (mode) {
            case 0:
                fc.position(fc.position() + offset);
                break;
            case 1:
                fc.position(offset);
                break;
            case 2:
                fc.position(fc.size() + offset);
                break;
            default:
                return (-1);
        }
        // Clear interrupt status to avoid unnecessarily closing the stream.
        Thread.interrupted();
        return fc.position();
    }

    public static boolean fdReady(Channel c, boolean write, int msecs) {
        if (c instanceof SelectableChannel) {
            try {
                Selector s             = Selector.open();
                SelectableChannel sc   = (SelectableChannel) c;
                // Clear interrupt status to avoid unnecessarily closing the stream.
                Thread.interrupted();
                SelectionKey selectKey = sc.register(s,
                                                     write? SelectionKey.OP_WRITE
                                                          : SelectionKey.OP_READ);
                if (msecs > 0) {
                    return (s.select(msecs) > 0);
                } else {
                    return (s.selectNow() > 0);
                }
            } catch (IOException e) {
                return true;
            }
        }
        return true;
    }

    public static int c_rand() {
        return (int)(Math.random() * 32768.0);
    }

    public static int cmp_thread(TSO t1, TSO t2) {
        int id1 = t1.id;
        int id2 = t2.id;
        if (id1 == id2) return 0;
        else if (id1 > id2) return 1;
        else return -1;
    }

    public static Closure jstringToString(StgContext context, String str) {
        int off = 0;
        int len = str.length();
        int codepoint = 0;
        ZC prevCurrent = null;
        ZC current = new ZC(null, null);
        ZC head    = current;
        for (off = 0;
             off < len;
             off += Character.charCount(codepoint)) {
            codepoint = str.codePointAt(off);
            current.x1 = new Czh(codepoint);
            ZC next = new ZC(null, null);
            current.x2 = next;
            prevCurrent = current;
            current = next;
        }
        if (head.x1 == null) return Types.DZMZN();
        prevCurrent.x2 = Types.DZMZN();
        return head;
    }

    public static Path getPath(String path) {
        return Paths.get(path);
    }

    public static FileChannel
    fileChannelOpen(Path path, Set<OpenOption> options,
                    FileAttribute<Set<PosixFilePermission>> attribute)
            throws IOException {
        // checks where file store for our path does know about POSIX
        Set<String> faViews = FileSystems.getDefault().supportedFileAttributeViews();
        if (faViews.contains("posix")) {
            return FileChannel.open(path, options, attribute);
        }

        // default behaviour -- create file, use old file api to set permissions
        // return and open channel
        File fChan = path.toFile();
        if(options.contains(StandardOpenOption.CREATE_NEW)) {
            // creates new file
            boolean created = fChan.createNewFile();
            if (!created) {
                throw new IOException("Could not create file " + fChan.getAbsolutePath());
            }
        }

        Set<PosixFilePermission> perms = attribute.value();
        fChan.setExecutable(
                perms.contains(PosixFilePermission.OWNER_EXECUTE),
                perms.contains(PosixFilePermission.GROUP_EXECUTE) || perms.contains(PosixFilePermission.OTHERS_EXECUTE)
        );
        fChan.setWritable(
                perms.contains(PosixFilePermission.OWNER_WRITE),
                perms.contains(PosixFilePermission.GROUP_WRITE) || perms.contains(PosixFilePermission.OTHERS_WRITE)
        );
        fChan.setReadable(
                perms.contains(PosixFilePermission.OWNER_READ),
                perms.contains(PosixFilePermission.GROUP_READ) || perms.contains(PosixFilePermission.OTHERS_READ)
        );

        // prepare options, if file was crated --> removed from map
        HashSet<OpenOption> fOpts = new HashSet<>(options);
        fOpts.remove(StandardOpenOption.CREATE_NEW);

        return FileChannel.open(fChan.toPath(), fOpts);
    }

    public static final int dirSeparatorChar = File.separatorChar;

    public static void puts(String msg) {
        System.out.println(msg);
    }

    public static float log1pFloat(float f) {
      return (float)(Math.log1p(f));
    }
    public static float expm1Float(float f) {
      return (float)(Math.expm1(f));
    }

    public static void traceHeap(StgContext context, Closure c) {
        debugBelch("%s", c.toString());
    }

    public static String showRaw(final StgContext context, Closure closure) {
        if (closure instanceof Thunk) {
            /* TODO: Replace this with deep evaluate */
            closure = closure.evaluate(context);
        }
        if (closure instanceof DataCon) {
            return closure.toString();
        } else if (closure instanceof Function){
            return "<function>";
        } else if (closure instanceof PAP) {
            return "<partially applied function>";
        } else {
            return closure.getClass().getSimpleName() + "@" + System.identityHashCode(closure);
        }
    }
}
