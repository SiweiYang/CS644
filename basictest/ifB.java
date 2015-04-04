public class ifB {
    public ifB() {}
    public int test() {
        int x = 64;
        int ret = 0;
        int y = 32;
        if (x > y) {
            ret = x + y;
        }
        else {
            int a = 1;
            ret = a + x;
        }
        return ret;
    }
}
