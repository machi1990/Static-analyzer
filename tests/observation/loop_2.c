{
    int x;
    int y;
    
    y = 0;
    x = 0;
    
    while (rand(0,1) == 0) {
        if (x <= 50) {
            y = y + 1;
        } else {
            y = y - 1;
        }
        
        if (y < 0) {
            halt;
        }
        
        x  = x + 1;
        
        print(x,y);
    }

    print(x,y);
}