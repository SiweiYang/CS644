public class ifA {
    public ifA() {}
    public int test() {
        int x = 64;
        int ret = 0;
        int y = 32;
        if (x > y) {
            int a = y;
            int b = x;
            ret = b + y;
        }
        else {
            int a = 1;
            ret = a + x;
        }
        return ret;
    }
}
