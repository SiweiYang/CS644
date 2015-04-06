public class Array {
    public int length;
    public int pointer;
    public Array(int i){
        length = i;
        pointer = Runtime.malloc(i * 4);
    }
    public Array(int i, int j){
        length = i;
        pointer = j;
    }
    public int get(int i) {
        if (i >= length || i < 0) {
            Runtime.exception();
        }
    	return pointer + i * 4;
    }
}
