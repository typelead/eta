package eta.runtime;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class RuntimeOptions {
    private Properties p;

    public RuntimeOptions(String path) {
        load_properties(path);
    }

    /*
     * Loads properties from resource which
     * may be override by system properties
     */
    private void load_properties(String path) {
        Properties lp = new Properties();
        // Load properties from resource
        try (InputStream in = getClass().getClassLoader().getResourceAsStream(path)) {
            if (in != null) lp.load(in);
        } catch (IOException e) {
            e.printStackTrace();
        }
        Properties sp = System.getProperties();
        // Set loaded properties to be override by System properties
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
     * Get and parse value from properties.
     * Return default value if failed.
     */
    private Object getValue(String key, String type, Object d) {
        String val = p.getProperty(key);
        if (val == null) return d;
        try {
            switch (type) {
                case "INT":
                    return Integer.parseInt(val);
                case "DOUBLE":
                    return Double.parseDouble(val);
                case "BOOLEAN":
                    return Boolean.parseBoolean(val);
                default:
                    throw new IllegalArgumentException("Unsupported property type.");
            }
        } catch (NumberFormatException e) {
            return d;
        }
    }

    public int getInt(String key, int d) {
        return (int) getValue(key, "INT", d);
    }

    public double getDouble(String key, double d) {
        return (double) getValue(key, "DOUBLE", d);
    }

    public boolean getBoolean(String key, boolean d) {
        return (boolean) getValue(key, "BOOLEAN", d);
    }
}