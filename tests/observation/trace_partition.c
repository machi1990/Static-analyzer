{
    int x;
    int y;
    x = rand(10,15);
    y = 10;
    
    while (rand(0,1) == 0) {
        if (x == y){
            x = x + 1;
            y = x;
            print(x,y);
        }
        else {
            x = x -1 ;
            y = y + 1;
        }
    }
    
    print(x,y);
}

