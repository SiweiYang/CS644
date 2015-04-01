public class Array {
    public int length;
    public int pointer;
    public Array(int i){
	length = i;
        pointer = Runtime.malloc(i);
    }
    public int get(int i) {
        //pointer = Runtime.malloc(i);
	return pointer + i;
    }
}
