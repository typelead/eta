package eta.runtime;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class RuntimeOptions{
    public static final String RTS_PROPERTIES_PATH = "eta/rts.properties";
    /*
     * Loads properties from resource which
     * may be override by system properties
     */
    public static Properties load_properties(){
        Properties p = new Properties();
        // Load properties from resource
        try(InputStream in = RuntimeOptions.class.getClassLoader().getResourceAsStream(RTS_PROPERTIES_PATH)){
            if(in != null) p.load(in);
        }catch(IOException e){
            e.printStackTrace();
        }
        Properties sp = System.getProperties();
        // Set loaded properties to be override by System properties
        for(String key : sp.stringPropertyNames()){
            p.setProperty(key,sp.getProperty(key));
        }
        return p;
    }
    /*
     * Get and parse value from properties.
     * Return default value if failed.
     */
    public static Object getValue(Properties p, String key, String type, Object d){
        String val = p.getProperty(key);
        if(val==null) return d;
        try{
            switch(type){
                case "Integer": return Integer.parseInt(val);
                case "Double": return Double.parseDouble(val);
                case "Float": return Float.parseFloat(val);
                case "Boolean": return Boolean.parseBoolean(val);
                default: throw new IllegalArgumentException("Unsupported property type.");
            }
        }catch(NumberFormatException e){
            return d;
        }
    }
}