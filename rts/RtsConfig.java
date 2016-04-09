package ghcvm.runtime;

public class RtsConfig {
    public enum RtsOpsEnabled {
        None,
        SafeOnly,
        All
    }

    public RtsOpsEnabled rtsOptsEnabled;
    public String rtsOpts;
    public boolean rtsHsMain;

    public RtsConfig() {}

    public static RtsConfig getDefault() {
        RtsConfig config = new RtsConfig();
        config.rtsOptsEnabled = RtsOpsEnabled.SafeOnly;
        config.rtsOpts = null;
        config.rtsHsMain = false;
        return config;
    }
}
