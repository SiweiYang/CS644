public class whileB {
    public whileB() {}
    public int test() {
        int x = 10;
        int ret = 0;
        int y = 20;
        while (x < y) {
            int c = 1;
            ret = ret + x;
            int d = 2;
            x = x + 1;
        }
        return ret;
    }
}
