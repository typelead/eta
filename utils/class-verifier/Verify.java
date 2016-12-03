import java.io.File;
import java.io.IOException;

public class Verify {
    public static void main(String[] args) {
        boolean error = false;
        try {
            error = getAllFiles(args[0].replaceAll("/",".") + ".", new File(args[0]).getCanonicalFile());
        } catch (IOException e) {
            error = true;
            e.printStackTrace();
        }
        if (error) System.exit(1);
    }

    private static boolean getAllFiles(String prefix, File curDir) {
        boolean error = false;
        File[] filesList = curDir.listFiles();
        for(File f : filesList){
            if(f.isFile()){
                String className = prefix + f.getName().replaceAll(".class","");
                //System.out.println(className);
                try {
                    Class.forName(className);
                } catch (ClassFormatError e) {
                    error = true;
                    System.out.println(className);
                    e.printStackTrace();
                } catch (ClassNotFoundException e) {
                    error = true;
                    System.out.println(className);
                    e.printStackTrace();
                } catch (VerifyError e) {
                    error = true;
                    System.out.println(className);
                    e.printStackTrace();
                } catch (NoClassDefFoundError e) {
                    error = true;
                    System.out.println(className);
                    e.printStackTrace();
                }
            }
        }
        return error;
    }
}
