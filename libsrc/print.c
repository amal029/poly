#include<stdio.h>

extern void p1 (int a, int* ret){
  fprintf(stdout,"Got a single value: %d\n",a);
  *ret = 1;
}

extern void p2 (int a, int b, int *ret){
  fprintf (stdout, "Got values: %d, %d\n", a, b);
  *ret = 0;
}

extern void print_array (int size, int A[size], int *ret){
  int i =0;
  for (;i<size;++i)
    fprintf(stdout,"%d\t",A[i]);
  fprintf(stdout,"\n");
  *ret = i;
}

extern void print_array2 (int size, int A[size][size], int *ret){
  int i =0,j =0;
  for (;i<size;++i)
    for(j=0;j<size;++j)
      fprintf(stdout,"%d\t",A[i][j]);
  fprintf(stdout,"\n");
  *ret = (i+j);
}

extern void print_array3 (int size, int A[size][size][size], int *ret){
  int i =0,j =0,k=0;
  for (;i<size;++i)
    for(j=0;j<size;++j)
      for(k=0;k<size;++k)
      fprintf(stdout,"%d\t",A[i][j][k]);
  fprintf(stdout,"\n");
  *ret = (i+j+k);
}
