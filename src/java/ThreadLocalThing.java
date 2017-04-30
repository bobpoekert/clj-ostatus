import clojure.lang.IFn;
import clojure.lang.IDeref;
import java.lang.ThreadLocal;


public class ThreadLocalThing extends ThreadLocal implements IDeref {

    final Object sentinelValue;
    final IFn generator;

    public ThreadLocalThing(Object sentinelValue, IFn generator) {
        this.sentinelValue = sentinelValue;
        this.generator = generator;
    }

    @Override
    public Object initialValue() {
        return this.sentinelValue;
    }

    public Object deref() {
        Object res = this.get();
        if (res == this.sentinelValue) {
            res = this.generator.invoke();
            this.set(res);
        }
        return res;
    }


}
