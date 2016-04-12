package ghcvm.runtime;

import ghcvm.runtime.types.*;

public class RtsTaskManager {
    public static Task allTasks;
    public static int taskCount;
    public static int workerCount;
    public static int currentWorkerCount;
    public static int peakWorkerCount;
    private static boolean tasksInitialized;
    public static final ThreadLocal<Task> myTask = new ThreadLocal<Task>() {
            @Override
            protected Integer initialValue() {
                return null;
            }
        };
    public static void init() {
        if (!tasksInitialized) {
            taskCount = 0;
            workerCount = 0;
            currentWorkerCount = 0;
            peakWorkerCount = 0;
            tasksInitialized = true;
        }
    }
    public static void startWorkerTasks(int from, int to) {
        for (int i = from; i < to; i++) {
            cap = capabilities.get(i);
            synchronized (cap) {
                cap.startWorkerTask();
            }
        }
    }
}
