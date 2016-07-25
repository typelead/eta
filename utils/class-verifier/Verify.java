import java.io.File;
import java.io.IOException;

public class Verify {
    public static void main(String[] args) {
        try {
            getAllFiles(args[1], new File(args[0]).getCanonicalFile());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void getAllFiles(String prefix, File curDir) {
        File[] filesList = curDir.listFiles();
        for(File f : filesList){
            if(f.isFile()){
                String className = prefix + f.getName().replaceAll(".class","");
                System.out.println(className);
                try {
                    Class.forName(className);
                } catch (ClassFormatError e) {
                    e.printStackTrace();
                } catch (ClassNotFoundException e) {
                    e.printStackTrace();
                } catch (VerifyError e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
