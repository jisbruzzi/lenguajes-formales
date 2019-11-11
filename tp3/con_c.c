#include <stdio.h>
int main()
{ int x=1,z=0;
    z =  (x = x * 3) + (x = x + 7) + (x = x + 1);
    //  da lo mismo que si fuese   z =  (x = z / 2) + x;
    printf("z= %d  ", z);
    printf("x= %d  ", x);
    return 0;
}