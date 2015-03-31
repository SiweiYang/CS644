public class NativeArray {
    public int length;
    public int pointer;
    public NativeArray(int i){
	length = i;
        pointer = Runtime.malloc(i);
    }
    public int get(int i) {
        //pointer = Runtime.malloc(i);
	return pointer + i;
    }
}
