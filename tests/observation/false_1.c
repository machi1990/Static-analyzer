{
    int a;
    int b;
    int c;

    a = 9;
    b = 7;

    if (1 < 1) {
        c = b;
        b = a;
        a = c;
    } else {
        c = b;
        b = a;
        a = c;
    }

    print(a,b); // a = 7; b = 9;
}
