int foo(int a) {
    if (a > 5) {
        return a / 5;
    }
    return a;
}

int main() {
    int a = foo(3);
    int b = foo(7);
    return a * b;
}
