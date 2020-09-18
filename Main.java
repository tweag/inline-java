import java.lang.reflect.Method;
import java.util.*;

public class Main {

    public static void main(String[] aaargs) throws Exception {

        Class c = Class.forName("java.lang.Class");
        for (String[] args : new String[][]{
            new String[]{"getMethods", "()[Ljava/lang/reflect/Method;"},
            new String[]{"currenntThread", "()Ljava/lang/Thread;"},
            new String[]{"currentThread", "()Ljava/lang/Object;"},
        }){
            try {
                Method m = Wrappers.getMethod(c, args[0], args[1]);
                System.out.println(m);
            } catch (Exception e) {
                System.out.println(e.toString());
            }
        }
    }
}

final class Wrappers {
    public static Method getMethod(Class klass, String methodName, String jniSignature) throws VerboseNoSuchMethodException {
        List<String> candidates = new ArrayList<String>();
        for (Method m : klass.getMethods()) {
            if (!m.getName().equals(methodName)) continue;
            String sig = JNIUtil.getJNIMethodSignature(m);
            if (sig.equals(jniSignature)) return m;
            candidates.add(sig);
        }
        throw new VerboseNoSuchMethodException(klass, methodName, jniSignature, candidates);
    }
}

final class VerboseNoSuchMethodException extends Exception {
    List<String> candidates;
    String signature;
    String methodName;
    Class klass;
    
    VerboseNoSuchMethodException(Class klass, String methodName, String signature, List<String> candidates) {
        this.methodName = methodName;
        this.signature = signature;
        this.candidates = candidates;
        this.klass = klass;
    }

    public String toString() {
        String ret = String.format("%s: %s %s\nPossible signatures for %s in %s:",
            super.toString(),
            methodName,
            signature,
            methodName,
            klass.getName()
        );
        if (candidates.isEmpty()) {
            ret += "\n  None";
        }
        else {
            for (String sig : candidates) {
                ret += "\n  " + sig;
            }
        }
        return ret;
    }
}

/**    Commodity utility for JNI
 * TODO: Author and license at https://www.codeproject.com/Tips/1129615/JNI-Signature-for-Java-Method
 */
final class JNIUtil
{
    private static final Map<Object, String> PRIMITIVE_SIGNATURES = new HashMap<>();
    static
    {
        PRIMITIVE_SIGNATURES.put(boolean.class, "Z");
        PRIMITIVE_SIGNATURES.put(byte.class, "B");
        PRIMITIVE_SIGNATURES.put(char.class, "C");
        PRIMITIVE_SIGNATURES.put(double.class, "D");
        PRIMITIVE_SIGNATURES.put(float.class, "F");
        PRIMITIVE_SIGNATURES.put(int.class, "I");
        PRIMITIVE_SIGNATURES.put(long.class, "J");
        PRIMITIVE_SIGNATURES.put(short.class, "S");
        PRIMITIVE_SIGNATURES.put(void.class, "V");
    }
    
    private JNIUtil()    {}
        
    /**    Build JNI signature for a method
     * @param m
     * @return
     */
    public static final String getJNIMethodSignature(Method m)
    {
        final StringBuilder sb = new StringBuilder("(");
        for(final Class<?> p : m.getParameterTypes())
        {
            sb.append(getJNIClassSignature(p));
        }
        sb.append(')').append(getJNIClassSignature(m.getReturnType()));
        return sb.toString();
    }

    /**    Build JNI signature from a class
     * @param c
     * @return
     */
    static String getJNIClassSignature(Class<?> c)
    {
        if(c.isArray())
        {
            final Class<?> ct = c.getComponentType();
            return '[' + getJNIClassSignature(ct);
        }
        else if(c.isPrimitive())
        {
            return PRIMITIVE_SIGNATURES.get(c);
        }
        else
        {
            return 'L' + c.getName().replace('.', '/') + ';';
        }        
    }
}