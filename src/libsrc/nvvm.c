/* This is the file that gives the ABI (Application binary interface)
   for communication with the PTX generator and the Nvidia GPUs */


/* Version: 1.0 
   Author: Avinash Malik
   Date: Sun Oct 28 16:41:03 GMT 2012
*/


/* FIXME: Currently we can only run one kernel at a time */


#include <math.h>
#include <cuda.h>
#include <builtin_types.h>
#include <drvapi_error_string.h>
#include "nvvm.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/queue.h>
#include <stdarg.h>


enum Errors {platform,memory,filename,noninit};

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
  case noninit :
    fprintf(stderr,"INIT NOT CALLED ON ABI: %s,\t line: %i\n",file, line);
  default: break;
  }
  exit (-1);
}

struct arg {
  /* The actual pointer to data */
  void *data;
  /* The size  */
  size_t memsize;
  /* The list pointers */
  LIST_ENTRY (arg) enteries;
};

struct device_ptrs {
  CUdeviceptr d_data;
  LIST_ENTRY (device_ptrs) devices;
};

LIST_HEAD (dev_list, device_ptrs) devs = 
  LIST_HEAD_INITIALIZER (devs);

LIST_HEAD (in_list, arg) ins =
  LIST_HEAD_INITIALIZER(ins);

LIST_HEAD (out_list, arg) outs =
  LIST_HEAD_INITIALIZER(outs);

#define checkCudaErrors(err)  __checkCudaErrors (err, __FILE__, __LINE__)
int launch (size_t, size_t, size_t, const char *, const char*);
void __checkCudaErrors( CUresult err, const char *file, const int line );

/* First initialize the linked list */
void
POLY_INIT(){
  static unsigned char done = 0;
  if (!done) {
    LIST_INIT (&ins);
    LIST_INIT (&outs);
    LIST_INIT (&devs);
    done = 1;
  }
}

void
poly_set_param (void *data, size_t memsize, struct in_list *i, struct out_list *o) {
  struct arg* n = malloc (sizeof (struct arg));
  n->data = data;
  n->memsize = memsize;
  struct arg* p = NULL;
  if (i != NULL) {
    if ((p = LIST_FIRST(i)) == NULL ) {
      POLY_INIT ();
      LIST_INSERT_HEAD(i, n, enteries);
    }
    else {
      while ((p=LIST_NEXT(p,enteries)) != NULL) ;
      /* Now we have the pointer in hand */
      LIST_INSERT_AFTER (p,n,enteries);
    }
  }
  else if (o!=NULL) {
    if ((p = LIST_FIRST(o)) == NULL ) {
      POLY_INIT ();
      LIST_INSERT_HEAD(i, n, enteries);
    }
    else {
      while ((p = LIST_NEXT(p,enteries)) != NULL) ;
      /* Now we have the pointer in hand */
      LIST_INSERT_AFTER (p,n,enteries);
    }
  }
  /* Traverse */
  else __report_errors (noninit,__FILE__,__LINE__);
}

void
poly_delete_param (struct in_list *i, struct out_list *o) {
  struct arg* p = NULL;
  if (i != NULL) {
    if (!LIST_EMPTY (i)) {
      p = LIST_FIRST (i);
      LIST_REMOVE (p,enteries);
      free (p);
    }
  }
  else if (o != NULL) {
    p = LIST_FIRST (o);
    LIST_REMOVE (p,enteries);
    free (p);
  }
}


/* This function actually runs the whole kernel on CUDA */
void
poly_run_kernel (size_t p1, size_t p2, size_t p3, const char *filename, const char *kernel) {
  launch (p1,p2,p3,filename, kernel);
}

size_t
get_size (struct arg *p) {
  return (p->memsize)/8;
} 

/* Allocates memory on the device */
void
assign_dev_mem () {
  /* First we put the device memory online */
  struct arg *p= NULL;
  LIST_FOREACH (p,&ins,enteries) {
    size_t memsize = get_size (p);
    struct device_ptrs *dev_t = malloc (sizeof (struct device_ptrs));
    dev_t->d_data = 0;
    LIST_INSERT_HEAD (&devs,dev_t,devices);
    checkCudaErrors (cuMemAlloc (&(dev_t->d_data),memsize));
    checkCudaErrors (cuMemcpyHtoD(dev_t->d_data,p->data,memsize));
  }
  p = NULL;
  LIST_FOREACH (p,&outs,enteries) {
    size_t memsize = get_size (p);
    struct device_ptrs *dev_t = malloc (sizeof (struct device_ptrs));
    dev_t->d_data = 0;
    LIST_INSERT_HEAD (&devs,dev_t,devices);
    checkCudaErrors (cuMemAlloc (&(dev_t->d_data),memsize));
    checkCudaErrors (cuMemcpyHtoD(dev_t->d_data,p->data,memsize));
  }
}

/* THE ABI */

void POLY_REGISTER_INPUT(void *d, int s){poly_set_param (d,s,&ins,NULL);}
void POLY_REGISTER_OUTPUT(void *d, int s){poly_set_param (d,s,NULL,&outs); }
void POLY_DEREGISTER_INPUTS (){ poly_delete_param (&ins,NULL);}
void POLY_DEREGISTER_OUTPUTS (){poly_delete_param (NULL,&outs);}
void POLY_LAUNCH_KERNEL (const char *f, const char *t, int p1, int p2, int p3){poly_run_kernel (p1,p2,p3,f,t);}

#include "cuda_kernel_launch.c"


