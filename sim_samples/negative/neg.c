int get_neg(int a) {
    return -a;
}


int main() {
    int a = get_neg(9);
    int b = get_neg(-9);
    int res = a + b;
    return res;
}
