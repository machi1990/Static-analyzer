{
    int x;
    int y;
    
    x = 0;
    y = rand(47,55);
    
    while (y < 50) {
        if ((x % 2) == 0) {
            y = y + 1;
        }
        
        x = x + 4;
        
    }
    
    print(x,y);
}