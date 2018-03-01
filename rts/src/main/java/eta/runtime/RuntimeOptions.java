package eta.runtime;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class RuntimeOptions {
    private Properties p;

    public RuntimeOptions(String path) {
        loadProperties(path);
    }

    /*
     * Loads properties from resource which
     * may be override by system properties
     */
    private void loadProperties(String path) {
        Properties lp = new Properties();
        // Load properties from resource
        try (InputStream in = getClass().getClassLoader().getResourceAsStream(path)) {
            if (in != null) lp.load(in);
        } catch (IOException e) {
            e.printStackTrace();
        }
        Properties sp = System.getProperties();
        /*
         * Add properties loaded from resource to system properties
         * without overriding existing ones
         */
        for (String key : lp.stringPropertyNames()) {
            if (sp.getProperty(key) == null) {
                sp.setProperty(key, sp.getProperty(key));
            }
        }
        p = sp;
    }

    public Properties getProperties() {
        return p;
    }

    /*
     * The following methods get and parse value from properties
     * and return the provided default value if failed.
     */

    public int getInt(String key, int d) {
        String val = p.getProperty(key);
        if (val == null) return d;
        try {
            return Integer.parseInt(val);
        } catch (NumberFormatException e) {
            return d;
        }
    }

    public double getDouble(String key, double d) {
        String val = p.getProperty(key);
        if (val == null) return d;
        try {
            return Double.parseDouble(val);
        } catch (NumberFormatException e) {
            return d;
        }
    }

    public boolean getBoolean(String key, boolean d) {
        String val = p.getProperty(key);
        if (val == null) return d;
        // return true only if val equals "true"(case-insensitive) and false otherwise
        return Boolean.parseBoolean(val);
    }
}