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
import eta.runtime.RuntimeLogging;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Closure;
import eta.runtime.io.MemoryManager;

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

    // TODO: Verify
    public static boolean isPrintableChar(int c) {
        return ((((1 << Character.UPPERCASE_LETTER)          |
                  (1 << Character.LOWERCASE_LETTER)          |
                  (1 << Character.TITLECASE_LETTER)          |
                  (1 << Character.MODIFIER_LETTER)           |
                  (1 << Character.OTHER_LETTER)              |
                  (1 << Character.COMBINING_SPACING_MARK)    |
                  (1 << Character.OTHER_NUMBER)              |
                  (1 << Character.MODIFIER_SYMBOL)           |
                  (1 << Character.ENCLOSING_MARK)            |
                  (1 << Character.DECIMAL_DIGIT_NUMBER)      |
                  (1 << Character.DASH_PUNCTUATION)          |
                  (1 << Character.OTHER_PUNCTUATION)         |
                  (1 << Character.CONNECTOR_PUNCTUATION)     |
                  (1 << Character.MATH_SYMBOL)               |
                  (1 << Character.SPACE_SEPARATOR)           |
                  (1 << Character.OTHER_SYMBOL)              |
                  (1 << Character.END_PUNCTUATION)           |
                  (1 << Character.FINAL_QUOTE_PUNCTUATION)   |
                  (1 << Character.START_PUNCTUATION)         |
                  (1 << Character.CURRENCY_SYMBOL)           |
                  (1 << Character.INITIAL_QUOTE_PUNCTUATION) |
                  (1 << Character.LETTER_NUMBER)             |
                  (1 << Character.LETTER_NUMBER)) >> Character.getType(c)) & 1)
            != 0;
    }

    public static boolean isFloatNegativeZero(float f) {
        return f == -0.0f;
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
        return d == -0.0;
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

    public static int c_write(Channel fd, long address, int count) {
        try {
            WritableByteChannel wc     = (WritableByteChannel) fd;
            ByteBuffer          buffer = MemoryManager.getBoundedBuffer(address);
            buffer.limit(buffer.position() + count);
            return wc.write(buffer);
        } catch (Exception e) {
            e.printStackTrace();
            return -1;
        }
    }

    public static int c_read(Channel fd, long address, int count) {
        try {
            ReadableByteChannel rc     = (ReadableByteChannel) fd;
            ByteBuffer          buffer = MemoryManager.getBoundedBuffer(address);
            buffer.limit(buffer.position() + count);
            int size = rc.read(buffer);
            if (size < 0) return 0;
            else return size;
        } catch (Exception e) {
            e.printStackTrace();
            return -1;
        }
    }

    public static String byteBufferToStr(long address, int len)
        throws UnsupportedEncodingException {
        return new String(eta.ghc_prim.Utils.byteBufferToBytes(address, len), "UTF-8");
    }

    public static String getOS() {
        return System.getProperty("os.name");
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

    /* Returns CPU time in picoseconds */
    public static long getCPUTime() {
        return ((OperatingSystemMXBean)
                ManagementFactory.getOperatingSystemMXBean())
               .getProcessCpuTime()
               * 1000;
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
        contents.limit(contents.position() + len);
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

    public static Map<Object, Integer> fileLocks = new HashMap<Object, Integer>();

    public static synchronized boolean lockFile(Object key, boolean forWriting) {
        Integer readers = fileLocks.get(key);
        if (readers == null) {
            int readersInt = forWriting? -1 : 1;
            fileLocks.put(key, readersInt);
        } else {
            if (forWriting || readers < 0) return false;
            fileLocks.put(key, readers + 1);
        }
        return true;
    }

    public static synchronized boolean unlockFile(Object key) {
        Integer readers = fileLocks.get(key);
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

    public static void setNonBlockingFD(Channel c, boolean blocking) throws IOException {
        if (c instanceof SelectableChannel) {
            ((SelectableChannel) c).configureBlocking(blocking);
        }
    }

    public static long c_lseek(FileChannel fc, long offset, int mode) throws IOException {
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
        return fc.position();
    }

    public static boolean fdReady(Channel c, boolean write, int msecs) {
        if (c instanceof SelectableChannel) {
            try {
                Selector s             = Selector.open();
                SelectableChannel sc   = (SelectableChannel) c;
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

    public static final int pathSeparatorChar = File.pathSeparatorChar;

    public static void puts(String msg) {
        System.out.println(msg);
    }
}
