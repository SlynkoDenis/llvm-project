int main() {
    int res = 0;
    for (int i = 0; i < 100; ++i) {
        res += i - i / 2;
    }

    return res;
}
