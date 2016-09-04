import java.io.File;
import java.io.IOException;

public class Verify {
    public static void main(String[] args) {
        try {
            getAllFiles(args[0].replaceAll("/",".") + ".", new File(args[0]).getCanonicalFile());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void getAllFiles(String prefix, File curDir) {
        File[] filesList = curDir.listFiles();
        for(File f : filesList){
            if(f.isFile()){
                String className = prefix + f.getName().replaceAll(".class","");
                //System.out.println(className);
                try {
                    Class.forName(className);
                } catch (ClassFormatError e) {
                    System.out.println(className);
                    e.printStackTrace();
                } catch (ClassNotFoundException e) {
                    System.out.println(className);
                    e.printStackTrace();
                } catch (VerifyError e) {
                    System.out.println(className);
                    e.printStackTrace();
                } catch (NoClassDefFoundError e) {
                    System.out.println(className);
                    e.printStackTrace();
                }
            }
        }
    }
}
