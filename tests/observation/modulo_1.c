{
    int x;
    int y;
    int z;

    x = rand(0,10);

    if (x > 5) {
        z = (rand(3,49) + x) % x; // z in [0,9]
        print(z);
    } else {
        z = 10-x;
        z = z % x;
        print(z); // z in [0,4]
    }

    y = rand(2,7);

    z = (x % y);

    print(z); // z in [0,7]
}
