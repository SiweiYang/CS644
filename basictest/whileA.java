public class whileA {
    public whileA() {}
    public int test() {
        int ret = 0;
        int x = 10;
        while (x < 20) {
            ret = ret + x;
            x = x + 1;
        }
        return ret;
    }
}
