public class Array {
    public int length;
    public int pointer;
    public Array(int i){
        length = i;
        pointer = Runtime.malloc(i);
    }
    public int get(int i) {
        if (i >= length || i < 0) {
            Runtime.exception();
        }
    	return pointer + i;
    }
}
