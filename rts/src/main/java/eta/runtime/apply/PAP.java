package eta.runtime.apply;

import eta.Function;
import eta.runtime.stg.Print;
import eta.Value;

public abstract class PAP extends Value {
    public Function fun;
    public int arity;
    protected PAP(Function fun, int arity) {
        this.fun = fun;
        this.arity = arity;
    }

    protected abstract void writeArgs(StringBuilder sb);

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        Class<? extends Function> clazz = fun.getClass();
        // TODO: Maybe make this a fully qualified name?
        sb.append('{');
        sb.append(Print.getClosureName(clazz));
        sb.append(' ');
        writeArgs(sb);
        sb.append('}');
        sb.append('[');
        sb.append(Integer.toString(arity));
        sb.append(']');
        return sb.toString();
    }
}
