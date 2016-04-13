{
    int x;
    int y;
    int z;
    
    y = 1;
    x = rand(-1,4);
    
    if ( y == x) {
        z = rand(10,19)/0;
        print(z);
    } else {
        z = x /rand (-1,2);
        print(z);
    }
    
    print(z);
}
