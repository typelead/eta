package ghcvm.runtime;

import static ghcvm.runtime.RtsFlags.RtsOptsEnabled;
import static ghcvm.runtime.RtsFlags.RtsOptsEnabled.RtsOptsSafeOnly;

public class RtsConfig {
    public RtsOptsEnabled rtsOptsEnabled;
    public String rtsOpts;
    public boolean rtsHsMain;

    public RtsConfig() {}

    public static RtsConfig getDefault() {
        RtsConfig config = new RtsConfig();
        config.rtsOptsEnabled = RtsOptsSafeOnly;
        config.rtsOpts = null;
        config.rtsHsMain = false;
        return config;
    }
}
