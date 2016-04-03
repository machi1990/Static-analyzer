{
    int x;
    x = rand(10,60);
    
    if ( x == 20) {
        x = x - rand (30,45);
    } else {
        x = x + rand (5,10);
    }
    
    print(x);
    
    while (x < 50) {
        x = x + 1;
    }
    
    print(x);
}