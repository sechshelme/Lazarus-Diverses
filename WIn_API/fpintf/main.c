
// x86_64-w64-mingw32-gcc main.c -o main

#include <stdio.h>

FILE *__iob_func(void);

int main(void)
{
    int zahl = 43;
    const char *text = "Hallo Welt";
    
    FILE *iob_array = __iob_func();
    
    FILE *my_stdout;
    
    my_stdout = iob_array + 1;                         
    
    fprintf(my_stdout, "Text: %s\n", text);
    fprintf(my_stdout, "Zahl: %d\n", zahl);

printf("size: %d\n", sizeof(FILE));


    return 0;
}
