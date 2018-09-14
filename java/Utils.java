package eta.java;

import java.io.IOException;
import java.io.File;
import java.io.FileOutputStream;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.Paths;
import java.nio.file.Files;
import java.nio.ByteBuffer;
import eta.runtime.io.MemoryManager;
import java.util.concurrent.atomic.AtomicInteger;

public class Utils {
  public static void touch(String path) {
    try {
      File f = new File(path);
      if (!f.exists()) {
        f.createNewFile();
      } else {
        f.setLastModified(System.currentTimeMillis());
      }
    } catch (IOException e) {
      // if an exception happens do nothing
    }
  }
  public static BasicFileAttributes getFileStatus(String path) {
    try {
      return Files.readAttributes(Paths.get(path), BasicFileAttributes.class);
    } catch(IOException e) {
      return null;
    }
  }
  public static int ptrStrLength(long address) {
    ByteBuffer buf = MemoryManager.getBoundedBuffer(address);
    int len = 0;
    while (buf.remaining() > 0 && buf.get() != 0) {
          len++;
    }
    return len;
  }
  private static AtomicInteger genSymCounter = new AtomicInteger();
  private static volatile int genSymInc = 1;

  public static int genSym() {
    return genSymCounter.getAndAdd(genSymInc) & 0xFFFFFF;
  }
  public static void initGenSym(int newGenSymCounter, int newGenSymInc) {
    genSymCounter.set(newGenSymCounter);
    genSymInc = newGenSymInc;
  }

}
