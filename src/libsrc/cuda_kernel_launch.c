// This will output the proper CUDA error strings in the event that a CUDA host call returns an error

// These are the inline versions for all of the SDK helper functions
void __checkCudaErrors( CUresult err, const char *file, const int line )
{
  if( CUDA_SUCCESS != err) {
    fprintf(stderr, "checkCudaErrors() Driver API error = %04d \"%s\" from file <%s>, line %i.\n",
	    err, getCudaDrvErrorString(err), file, line );
    exit(-1);
  }
}
/* This is using the CUDA driver API */
CUdevice cudaDeviceInit()
{
  /* This is the cuda device */
  CUdevice cuDevice = 0;
  /* The device count */
  int deviceCount = 0;
  /* Initialize the driver API */
  /* Just like MPI we always have to make this call */
  CUresult err = cuInit(0);
  if (CUDA_SUCCESS == err)
    checkCudaErrors(cuDeviceGetCount(&deviceCount));
  if (deviceCount == 0) {
    fprintf(stderr, "cudaDeviceInit error: no devices supporting CUDA\n");
    exit(-1);
  }
  checkCudaErrors(cuDeviceGet(&cuDevice, 0));
  char name[100];
  cuDeviceGetName(name, 100, cuDevice);
  printf("Using CUDA Device [0]: %s\n", name);

  int major=0, minor=0;
  checkCudaErrors( cuDeviceComputeCapability(&major, &minor, cuDevice) );
  if (major < 2) {
    fprintf(stderr, "Device 0 is not sm_20 or later\n");
    exit(-1);
  }
  return cuDevice;
}


CUresult initCUDA(CUcontext *phContext,
                  CUdevice *phDevice,
                  CUmodule *phModule,
                  CUfunction *phKernel,
                  const char *ptx,
                  const  char *kname)
{
  // Initialize 
  /* Initialization using the driver API */
  *phDevice = cudaDeviceInit();

  // Create context on the device
  checkCudaErrors(cuCtxCreate(phContext, 0, *phDevice));

  // Load the PTX 
  checkCudaErrors(cuModuleLoadDataEx(phModule, ptx, 0, 0, 0));

  // Locate the kernel entry poin
  checkCudaErrors(cuModuleGetFunction(phKernel, *phModule, kname));

  return CUDA_SUCCESS;
}

char *loadProgramSource(const char *filename, size_t *size) 
{
  struct stat statbuf;
  FILE *fh;
  char *source = NULL;
  *size = 0;
  fh = fopen(filename, "rb");
  if (fh) {
    stat(filename, &statbuf);
    source = (char *) malloc(statbuf.st_size+1);
    if (source) {
      fread(source, statbuf.st_size, 1, fh);
      source[statbuf.st_size] = 0;
      *size = statbuf.st_size+1;
    }
  }
  else {
    fprintf(stderr, "Error reading file %s\n", filename);
    exit(-1);
  }
  return source;
}

char *generatePTX(const char *ll, size_t size)
{
  nvvmResult result;
  nvvmCU cu;

  result = nvvmInit();
  if (result != NVVM_SUCCESS) {
    fprintf(stderr, "nvvmInit: Failed\n");
    exit(-1);
  }

  result = nvvmCreateCU(&cu);
  if (result != NVVM_SUCCESS) {
    fprintf(stderr, "nvvmCreateCU: Failed\n");
    exit(-1); 
  }

  result = nvvmCUAddModule(cu, ll, size);
  if (result != NVVM_SUCCESS) {
    fprintf(stderr, "nvvmCUAddModule: Failed\n");
    exit(-1);
  }
 
  result = nvvmCompileCU(cu,  0, NULL);
  if (result != NVVM_SUCCESS) {
    fprintf(stderr, "nvvmCompileCU: Failed\n");
    size_t LogSize;
    nvvmGetCompilationLogSize(cu, &LogSize);
    char *Msg = (char*)malloc(LogSize);
    nvvmGetCompilationLog(cu, Msg);
    fprintf(stderr, "%s\n", Msg);
    free(Msg);
    nvvmFini();
    exit(-1);
  }
    
  size_t PTXSize;
  result = nvvmGetCompiledResultSize(cu, &PTXSize);
  if (result != NVVM_SUCCESS) {
    fprintf(stderr, "nvvmGetCompiledResultSize: Failed\n");
    exit(-1);
  }
    
  char *PTX = (char*)malloc(PTXSize);
  result = nvvmGetCompiledResult(cu, PTX);
  if (result != NVVM_SUCCESS) {
    fprintf(stderr, "nvvmGetCompiledResult: Failed\n");
    free(PTX);
    exit(-1);
  }
    
  result = nvvmDestroyCU(&cu);
  if (result != NVVM_SUCCESS) {
    fprintf(stderr, "nvvmDestroyCU: Failed\n");
    free(PTX);
    exit(-1);
  }
    
  result = nvvmFini();
  if (result != NVVM_SUCCESS) {
    fprintf(stderr, "nvvmFini: Failed\n");
    free(PTX);
    exit(-1);
  }
    
  return PTX;
}

int launch (size_t p1, size_t p2, size_t p3, const char* fn, const char *argv) {
    
  CUcontext    hContext = 0;
  CUdevice     hDevice  = 0;
  CUmodule     hModule  = 0;
  CUfunction   hKernel  = 0;

  // Get the ll from file
  size_t size = 0;
  char *ll = NULL;
  ll = loadProgramSource(fn, &size);
#ifdef DEBUG
  fprintf(stdout, "NVVM IR ll file loaded\n");
#endif

  // Use libnvvm to generte PTX
  /* This is their compiler from ll to ptx */
  char *ptx = generatePTX(ll, size);
#ifdef PPTX
  fprintf(stdout, "PTX generated:\n");
  fprintf(stdout, "%s\n", ptx);
#endif
    
  // Initialize the device and get a handle to the kernel
  /* Initialize the CUDA device and the context */
  /* This also loads the module (the compiled ptx file and returns the
     pointer to the kernel function) */
  checkCudaErrors(initCUDA(&hContext, &hDevice, &hModule, &hKernel, ptx, argv));


  /* Now we need to make the call to the CUDA kernel */
  /* For now we assume that the blocks will result in equal sizes */
  
  if (p1 == 0) p1 = 1;
  if (p2 == 0) p2 = 1;
  if (p3 == 0) p3 = 1;
  int threadsperblockX, threadsperblockY, threadsperblockZ;
#ifdef NTPBX
  threadsperblockX = NTPBX;
#else
  if (p1 <= 1024)
    threadsperblockX = p1;
  else 
    threadsperblockX = 1024;
#endif
#ifdef NTPBY
  threadsperblockY = NTPBY;
#else
  if (p2 <= 1024)
    threadsperblockY = p2;
  else 
    threadsperblockY = 1024;
#endif
#ifdef NTPBZ
  threadsperblockZ = NTPBZ;
#else
  if (p3 <= 1024)
    threadsperblockZ = p3;
  else 
    threadsperblockZ = 1024;
#endif

  int blockspergridX = p1/threadsperblockX;
  int blockspergridY = p2/threadsperblockY;
  int blockspergridZ = p3/threadsperblockZ;
  
  /* Set the parameters for the kernel argument */
  /* The inputs come before outputs */
  struct device_ptrs *dev_t = NULL;
  size_t paramOffset = 0;
  size_t input_counter = 0;
  struct arg *p = NULL;
  LIST_FOREACH (p,&ins,enteries) ++input_counter;
  p = NULL;
  p = NULL;
  void *kernelParams [(input_counter-skip)+2];
  size_t c = 0;

  // Allocate memory on device and also transfer data to device
  LIST_INIT (&devs);
  assign_dev_mem();

#ifdef DEBUG
  size_t mcounter = 0;
  LIST_FOREACH (dev_t,&devs,devices) ++mcounter;
  fprintf(stdout,"DEV LIST SIZE: %d\n",mcounter);
  fflush(stdout);
#endif

  LIST_FOREACH (dev_t,&devs,devices) {
    kernelParams[c] = &(dev_t->d_data);
    ++c;
  }

#ifdef MYDEBUG
  printf("b/GX:%d\t b/GY:%d\t b/GZ: %d\nt/BX:%d\t t/BY:%d\t t/BZ:%d\n", 
	 blockspergridX, blockspergridY, blockspergridZ, threadsperblockX,
	 threadsperblockY, threadsperblockZ);
#endif

#ifdef TIME
  CUevent event1 = 0;
  CUevent event2 = 0;
  checkCudaErrors(cuEventCreate (&event1, CU_EVENT_DEFAULT|CU_EVENT_BLOCKING_SYNC));
  checkCudaErrors(cuEventCreate (&event2, CU_EVENT_DEFAULT|CU_EVENT_BLOCKING_SYNC));
  /* // Launch the kernel */
  checkCudaErrors(cuEventRecord(event1,0)); /* Start the start timer */
#endif


  checkCudaErrors(cuLaunchKernel(hKernel,blockspergridX,blockspergridY,blockspergridZ,
				 threadsperblockX, threadsperblockY, threadsperblockZ,
				 0,0,kernelParams,0));
#ifdef TIME
  checkCudaErrors(cuEventRecord(event2,0));
  /* Synchronize on both events */
  checkCudaErrors(cuEventSynchronize(event1));
  checkCudaErrors(cuEventSynchronize(event2));
  float time_taken = 0.0f;
  checkCudaErrors (cuEventElapsedTime(&time_taken,event1,event2));
  checkCudaErrors (cuEventDestroy(event1));
  checkCudaErrors (cuEventDestroy(event2));
  fprintf(stdout,"Total time taken for computation: %0.3f (msec)\n",time_taken);
  fflush(stdout);
#endif

#ifdef DEBUG
  printf("CUDA kernel launched\n");
#endif
    
  // Copy the result back to the host
  /* First get the device pointer to point to the output parts */
  dev_t = LIST_FIRST(&devs);
  c = 0;

  /* Need to skip here as well */
  size_t counter = 0;
  p = LIST_FIRST (&ins);
  while (counter < skip) {
    p = LIST_NEXT (p,enteries);
    ++counter;
  }
  /* Now assign memory back to host */
  do {
    checkCudaErrors(cuMemcpyDtoH((p->data), (dev_t->d_data), get_size (p)));
    dev_t = LIST_NEXT (dev_t,devices);
  }while((p=LIST_NEXT(p,enteries)) != NULL);

  /* Start the end timer */
  /* Cleanup */
  struct device_ptrs *dt = LIST_FIRST (&devs);
  while (dt != NULL ) {
    dev_t = LIST_NEXT (dt,devices);
    checkCudaErrors(cuMemFree(dt->d_data));
    dt->d_data = 0;
    free (dt);
    dt = dev_t;
  }

  /* We need to increment skip here */
  skip = input_counter;
  /* POLY_DEREGISTER_INPUTS(); */
  /* POLY_DEREGISTER_OUTPUTS(); */

  if (hModule) {
    checkCudaErrors(cuModuleUnload(hModule));
    hModule = 0;
  }
  if (hContext) {
    checkCudaErrors(cuCtxDestroy(hContext));
    hContext = 0;
  }

  free(ll);
  free(ptx);
    
  return 0;
}
