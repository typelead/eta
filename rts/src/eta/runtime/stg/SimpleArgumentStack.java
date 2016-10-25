package eta.runtime.stg;

import cern.colt.list.ObjectArrayList;

public class SimpleArgumentStack extends AbstractArgumentStack {

    public SimpleArgumentStack() {
        super();
    }

    public SimpleArgumentStack(final ObjectArrayList closures) {
        super(closures);
    }

    @Override
    public boolean isSimple() { return true; }
}


