/* This is the file that gives the ABI (Application binary interface)
   for communication with the PTX generator and the Nvidia GPUs */


/* Version: 1.0 
   Author: Avinash Malik
   Date: Sun Oct 28 16:41:03 GMT 2012
*/


#include <math.h>
#include <cuda.h>
#include <builtin_types.h>
#include <drvapi_error_string.h>
#include "nvvm.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#include <stdarg.h>

enum Errors {platform,memory,filename};

enum Types {UINT=0, 
	    INT=1, 
	    FLOAT=2, 
	    DOUBLE=3, 
	    ULL=4};

#ifdef __linux__
#else __report_errors (paltform, __FILE__, __LINE__)
#endif

void
__report_errors (enum Errors err, const char *file, const int line){

  switch (err){
  case platform:
    fprintf(stderr,"Only Linux and OSX currently supported %s,\t line: %i\n", file, line);
    break;
  case memory:
    fprintf(stderr,"Not enough memory: %s,\t line: %i\n", file, line);
  case filename:
    fprintf(stderr,"No file to load: %s,\t line: %i\n",file, line);
  default: break;
  }
  exit (-1);
}

struct arg {
  enum Types type;		/* The type of the input argument */
  /* Each input can be of size 65535 */
  /* x dimension on CUDA */
  /* y dimension on CUDA */
  /* z dimension on CUDA */
  int dim [3]; 		
  /* The actual pointer to data */
  void *data;
};

/* We can have a number of input and output arguments for any given CUDA
   call */
struct arg *inputs = NULL;
struct arg *outputs = NULL;
size_t input_size = -1;
size_t output_size = -1;

/* The number of definitions */
#define POLY_INPUT_PARAM_INIT (t) poly_kernel_initialize (input_size,inputs,t)
#define POLY_OUTPUT_PARAM_INIT (t) poly_kernel_initialize (output_size,outputs,t)
#define POLY_INPUT_PARAM_TYPE (...) poly_param_type (input_size,inputs,__VA_ARGS__)
#define POLY_OUTPUT_PARAM_TYPE (...) poly_param_type (output_size,outputs,__VA_ARGS__)
#define POLY_INPUT_PARAM_SIZE (...) poly_param_size (input_size,inputs,__VA_ARGS__)
#define POLY_OUTPUT_PARAM_SIZE (...) poly_param_size (output_size,outputs,__VA_ARGS__)
#define POLY_INPUT_PARAM_VALUE (...) poly_param_size (input_size,inputs,__VA_ARGS__)
#define POLY_OUTPUT_PARAM_VALUE (...) poly_param_size (output_size,outputs,__VA_ARGS__)
#define POLY_INPUT_PARAM_CLEAR (t) poly_param_size (input_size,inputs)
#define POLY_OUTPUT_PARAM_CLEAR (t) poly_param_size (output_size,outputs)
#define POLY_RUN_KERNEL (t) poly_run_kernel (t)

/* Initialize the parameters for a kernel */
void
poly_kernel_initialize (size_t size, struct arg* value, size_t sval) {
  sval = size;
  if ((value = (struct arg*) calloc (sizeof(struct arg),size)) == NULL) {
    __report_errors (memory,__FILE__,__LINE__);
  }
}

/* Initialize the type of the parameters for a kernel */
void 
poly_param_type (size_t size, struct arg *value, enum Types t, ...) {

  va_list list;
  va_start (list, t);
  size_t i = 0;

  for (i=0;i<size;++i)
    value[i].type = (enum Types) va_arg (list,enum Types);

  va_end (list);
}

/* Initialize the size of the parameters for a kernel. One can pass a
   zero value in the index is not to be used.
*/
void 
poly_param_size (size_t size, struct arg *value, int dim_sizes [3], ...) {

  va_list list;
  va_start (list, dim_sizes);
  size_t i = 0;

  for (i=0;i<size;++i) {
    value[i].dim[0] = (int) va_arg (list,int*)[0];
    value[i].dim[1] = (int) va_arg (list,int*)[1];
    value[i].dim[2] = (int) va_arg (list,int*)[2];
  }

  va_end (list);
}


void
poly_param_value (size_t size, struct arg* value, void *t,...){

  va_list list;
  va_start (list, t);
  size_t i = 0;

  for (i=0;i<size;++i)
    value[i].data = (void*) va_arg (list,void*);

  va_end (list);  
}

void
poly_kernel_clear (size_t size, struct arg *v) {
  size = -1;
  free (v);
}

#include "cuda_kernel_launch.c"
/* This function actually runs the whole kernel on CUDA */
void
poly_run_kernel (const char *kernel) {
  launch (kernel);
}
