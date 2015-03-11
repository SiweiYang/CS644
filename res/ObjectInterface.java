package java.lang;
public interface ObjectInterface {
    public abstract boolean equals(Object other);
    public abstract String toString();
    public abstract int hashCode();
    protected abstract Object clone();
    public abstract final Class getClass();
}
