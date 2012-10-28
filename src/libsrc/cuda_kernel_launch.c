// This will output the proper CUDA error strings in the event that a CUDA host call returns an error
#define checkCudaErrors(err)  __checkCudaErrors (err, __FILE__, __LINE__)

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
                  const char *ptx)
{
    // Initialize 
  /* Initialization using the driver API */
    *phDevice = cudaDeviceInit();

    // Create context on the device
    checkCudaErrors(cuCtxCreate(phContext, 0, *phDevice));

    // Load the PTX 
    checkCudaErrors(cuModuleLoadDataEx(phModule, ptx, 0, 0, 0));

    // Locate the kernel entry poin
    checkCudaErrors(cuModuleGetFunction(phKernel, *phModule, "zeros1"));

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

int launch (const char *argv)
{
  if (argv == NULL) {__report_errors (filename,__FILE__,__LINE__);}
    
  /* Changed this to use just 1 thread */
    const unsigned int nThreads = 2;
    const unsigned int nBlocks  = 1;
    const size_t memSize = nThreads * nBlocks * sizeof(int);

    CUcontext    hContext = 0;
    CUdevice     hDevice  = 0;
    CUmodule     hModule  = 0;
    CUfunction   hKernel  = 0;
    CUdeviceptr  d_data   = 0;
    int         *h_data   = 0;

    // Get the ll from file
    size_t size = 0;
    char *ll = NULL;
    ll = loadProgramSource(argv, &size);
#ifdef DEBUG
    fprintf(stdout, "NVVM IR ll file loaded\n");
#endif

    // Use libnvvm to generte PTX
    /* This is their compiler from ll to ptx */
    char *ptx = generatePTX(ll, size);
#ifdef DEBUG
    fprintf(stdout, "PTX generated:\n");
    fprintf(stdout, "%s\n", ptx);
#endif
    
    // Initialize the device and get a handle to the kernel
    /* Initialize the CUDA device and the context */
    /* This also loads the module (the compiled ptx file and returns the
       pointer to the kernel function) */
    checkCudaErrors(initCUDA(&hContext, &hDevice, &hModule, &hKernel, ptx));

    // Allocate memory on host and device
    /* if ((h_data = (int *)malloc(memSize)) == NULL) { */
    /*     fprintf(stderr, "Could not allocate host memory\n"); */
    /*     exit(-1); */
    /* } */
    /* Allocate memory on the device */
    /* Need to also give the type and the size of the memloc to perform!! */
    /* This is the main function that needs to change */
    checkCudaErrors(cuMemAlloc(&d_data, memSize));

    // Set the kernel parameters
    /* The block shape is:
       32 threads in a line in a single block!!
      +----+----+----+---+---+---+----+----------+
      |    |    |    |   |   | 	 |    |	       	 |
      +----+----+----+---+---+---+----+----------+
     */
    checkCudaErrors(cuFuncSetBlockShape(hKernel, nThreads, 1, 1));
    int paramOffset = 0;
    checkCudaErrors(cuParamSetv(hKernel, paramOffset, &d_data, sizeof(d_data)));
    paramOffset += sizeof(d_data);
    checkCudaErrors(cuParamSetSize(hKernel, paramOffset));

    // Launch the kernel
    checkCudaErrors(cuLaunchGrid(hKernel, nBlocks, 1));
    fprintf(stdout, "CUDA kernel launched\n");
    
    // Copy the result back to the host
    checkCudaErrors(cuMemcpyDtoH(h_data, d_data, memSize));

    // Print the result
    for (unsigned int i = 0 ; i < nBlocks * nThreads ; i++) {
        fprintf(stdout, "%d ", h_data[i]);
    }

    fprintf(stdout, "\n");
    
    // Cleanup
    if (d_data) {
        checkCudaErrors(cuMemFree(d_data));
        d_data = 0;
    }
    if (h_data) {
        free(h_data);
        h_data = 0;
    }
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
