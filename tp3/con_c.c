#include <stdio.h>
int main()
{ int x=3,z=10;
    //z =  (x = x * 3) + (x = x + 7) + (x = x + 1);
    //z =  (x = x * 3) + (x = x + 7) + (x = x + 1);
    //z =  x + (x = z / 2);

    /*
    printf("z= %d  ", z);
    if (12 < (z = x + (x = z / 2))) {
        printf("si");
    } else {
        printf("no");
    }
    printf("z= %d  ", z);
    */
    //z =  x + (x = z / 5 * ( x = x + 1 ) );
    /*
    z =  (x = x + 2) + (x = z / 5 * ( x = x + 1 ) );
    
    printf("z= %d  ", z);
    printf("x= %d  ", x);
    */
    if ( (x = x * 2) == (x = x + 1) ) {
        printf("verdadero");
    } else {
        printf("falso");
    }
    printf("x= %d  ", x);

    return 0;
}