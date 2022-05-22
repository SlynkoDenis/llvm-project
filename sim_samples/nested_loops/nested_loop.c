int loop(int res) {
    int end1 = res * 2 / 3;
    int end2 = res * 3 / 4;

    for (int i = 0; i < end1; i++) {
        for (int j = 0; j < end2; j++) {
            res += i - j;
        }
    }
    return res;
}

int main() {
    int res = loop(35);
    return res;
}
