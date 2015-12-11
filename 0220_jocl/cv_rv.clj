(require 'cl)

(import '(org.jocl CL Sizeof Pointer cl_device_id cl_event))

(defn handle-cl-error [err-code]
  (when (not= err-code CL/CL_SUCCESS)
    (throw (Exception. (CL/stringFor_errorCode err-code)))))

(defn lcg [seed]
  (let [val (mod (+ (* seed 1103515245) 12345)
                 (bit-shift-left 1 32))]
    (cons val (lazy-seq (lcg val)))
    ))

(let [{pf :platform
       dev :device
       ctx :context
       q   :queue} (cl/context 'CL_DEVICE_TYPE_CPU)]
  (def platform (pf :id))
  (def devices [(dev :id)])
  (def context ctx)
  (def queue q))

(def N 4)

(defn prepare-mem [context]
  (let [err (int-array 1)
        cv-mem (CL/clCreateBuffer context CL/CL_MEM_READ_WRITE
                (* N Sizeof/cl_float) nil err)
        _ (handle-cl-error (first err))
        rv-mem (CL/clCreateBuffer context CL/CL_MEM_READ_WRITE
                (* N Sizeof/cl_float) nil err)
        _ (handle-cl-error (first err))
        prod-mem (CL/clCreateBuffer context CL/CL_MEM_READ_WRITE
                  (* N N Sizeof/cl_float) nil err)
        _ (handle-cl-error (first err))]
    {:cv cv-mem :rv rv-mem :prod prod-mem}))

(defn prepare-kernels [context devices]
  (let [src (slurp "cv_rv.cl")
        err (int-array 1)
        program (CL/clCreateProgramWithSource
                 context 1 (into-array String [src])
                 (long-array [(count src)]) err)
        _ (handle-cl-error (first err))
        er (CL/clBuildProgram
            program 1 (into-array cl_device_id devices) nil nil nil)
        _ (handle-cl-error er)
        kernel (CL/clCreateKernel program "cv_rv" err) 
        _ (handle-cl-error er)]
    {:program program :kernel kernel}))

(def errcode-ret (int-array 1))

;(def cv (map #(/ % 1.0 (bit-shift-left 1 32)) (take N (lcg 1))))
;(def cv (map #(- (mod % 19) 9) (take N (lcg 1))))
(def cv [1 2 3 4])
(def cv-array (float-array cv))

;(def rv (map #(- (mod % 19) 9) (take N (lcg 2))))
(def rv [2 3 4 5])
(def rv-array (float-array rv))

(let [{cv :cv rv :rv prod :prod} (prepare-mem context)]
  (def cv-mem cv)
  (def rv-mem rv)
  (def prod-mem prod))

(let [{kernel :kernel program :program} (prepare-kernels context devices)]
  (def program program)
  (def kernel kernel))

(print "clSetKernalArg for arg 0, errcode-ret = ")
(println
  (CL/clSetKernelArg
   kernel 0 Sizeof/cl_mem (Pointer/to prod-mem))
   )
(print "clSetKernalArg for arg 1, errcode-ret = ")
(println
  (CL/clSetKernelArg
   kernel 1 Sizeof/cl_mem (Pointer/to cv-mem))
   )
(print "clSetKernalArg for arg 2, errcode-ret = ")
(println
  (CL/clSetKernelArg
   kernel 2 Sizeof/cl_mem (Pointer/to rv-mem))
   )
(print "clSetKernalArg for arg 3, errcode-ret = ")
(println
  (CL/clSetKernelArg
   kernel 3 Sizeof/cl_int (Pointer/to (int-array [N])))
   )

(print "clEnqueueWriteBuffer for cv-mem, errcode-ret = ")
(println
  (CL/clEnqueueWriteBuffer
   queue cv-mem CL/CL_TRUE 0 (* N Sizeof/cl_float) (Pointer/to cv-array)
   0 nil nil)
   )
(print "clEnqueueWriteBuffer for rv-mem, errcode-ret = ")
(println
  (CL/clEnqueueWriteBuffer
   queue rv-mem CL/CL_TRUE 0 (* N Sizeof/cl_float) (Pointer/to rv-array)
   0 nil nil)
   )

(print "clEnqueueNDRangeKernel, errcode-ret = ")
(let [kernel-done (make-array cl_event 1)]
  (println
    (CL/clEnqueueNDRangeKernel
     queue kernel 2 nil (long-array [4 4]) (long-array [1 1])
     ;queue kernel 1 nil (long-array [16]) (long-array [1])
     ;queue kernel 1 nil (long-array [4]) (long-array [1])
     0 nil nil))
     )

(def prod-array (float-array (* N N)))


(CL/clEnqueueReadBuffer
 queue prod-mem CL/CL_TRUE 0 (* N N Sizeof/cl_float) (Pointer/to prod-array)
 0 nil nil)

(pprint cv-array)
(pprint rv-array)
(pprint (partition 4 prod-array))

(def hoge-array (float-array N))
(CL/clEnqueueReadBuffer
 queue cv-mem CL/CL_TRUE 0 (* N Sizeof/cl_float) (Pointer/to hoge-array)
 0 nil nil)
(pprint hoge-array)

(CL/clFlush queue)
(CL/clFinish queue)
(CL/clReleaseKernel kernel)
(CL/clReleaseProgram program)
(CL/clReleaseMemObject cv-mem)
(CL/clReleaseMemObject rv-mem)
(CL/clReleaseMemObject prod-mem)
(CL/clReleaseCommandQueue queue)
(CL/clReleaseContext context)
