import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

public class Verify {
    public static void main(String[] args) {
        List<String> failed = new ArrayList<String>();
        try {
            for (String arg: args) {
                failed.addAll(getAllFiles( arg.replaceAll("/",".") + "."
                                         , new File(arg).getCanonicalFile()));
            }
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
        if (failed.size() > 0) {
            System.out.println("The following classes failed verification:");
            for (String f: failed) {
                System.out.println("  - " + f);
            }
            System.exit(1);
        } else {
            System.out.println("All classes passed verification.");
        }
    }

    private static List<String> getAllFiles(String prefix, File curDir) {
        List<String> failed = new ArrayList<String>();
        File[] filesList = curDir.listFiles();
        for(File f : filesList){
            if(f.isFile()){
                String className = prefix + f.getName().replaceAll(".class","");
                //System.out.println(className);
                try {
                    Class.forName(className);
                } catch (ClassFormatError e) {
                    failed.add(className);
                    System.out.println(className);
                    e.printStackTrace();
                } catch (ClassNotFoundException e) {
                    failed.add(className);
                    System.out.println(className);
                    e.printStackTrace();
                } catch (VerifyError e) {
                    failed.add(className);
                    System.out.println(className);
                    e.printStackTrace();
                } catch (NoClassDefFoundError e) {
                    failed.add(className);
                    System.out.println(className);
                    e.printStackTrace();
                }
            }
        }
        return failed;
    }
}
