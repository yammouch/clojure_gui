(require 'cl)

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
        cv-mem (org.jocl.CL/clCreateBuffer
                context org.jocl.CL/CL_MEM_READ_WRITE
                (* N org.jocl.Sizeof/cl_float) nil err)
        _ (when (not= (first err) org.jocl.CL/CL_SUCCESS)
            (throw (Exception. (org.jocl.CL/stringFor_errorCode (first err)))))
        rv-mem (org.jocl.CL/clCreateBuffer
                context org.jocl.CL/CL_MEM_READ_WRITE
                (* N org.jocl.Sizeof/cl_float) nil err)
        _ (when (not= (first err) org.jocl.CL/CL_SUCCESS)
            (throw (Exception. (org.jocl.CL/stringFor_errorCode (first err)))))
        prod-mem (org.jocl.CL/clCreateBuffer
                  context org.jocl.CL/CL_MEM_READ_WRITE
                  (* N N org.jocl.Sizeof/cl_float) nil err)
        _ (when (not= (first err) org.jocl.CL/CL_SUCCESS)
            (throw (Exception. (org.jocl.CL/stringFor_errorCode (first err)))))
            ]
    {:cv cv-mem :rv rv-mem :prod prod-mem}))

(def errcode-ret (int-array 1))

;(def cv (map #(/ % 1.0 (bit-shift-left 1 32)) (take N (lcg 1))))
;(def cv (map #(- (mod % 19) 9) (take N (lcg 1))))
(def cv [1 2 3 4])
(def cv-array (float-array cv))

;(def rv (map #(- (mod % 19) 9) (take N (lcg 2))))
(def rv [2 3 4 5])
(def rv-array (float-array rv))

(let [src (slurp "cv_rv.cl")]
  (println src)
  (println (count src))
  (def program (org.jocl.CL/clCreateProgramWithSource
                context 1 (into-array String [src])
                (long-array [(count src)]) errcode-ret)))
(print "clCreateProgramWithSource, errcode-ret = ")
(println (nth errcode-ret 0))
;(let [buf (char-array 1024)]
;(def buf (char-array 1024))
(def buf (byte-array 1024))
(def len (long-array 1))
  (org.jocl.CL/clGetProgramInfo
   program org.jocl.CL/CL_PROGRAM_SOURCE 1024 (org.jocl.Pointer/to buf) len)
  (print "clGetProgramInfo, source = ")
  (print (apply str buf))
  (print ", len = ")
  (print (first len))
  ;)
(println (nth errcode-ret 0))

(org.jocl.CL/clBuildProgram
 program 1 (into-array org.jocl.cl_device_id devices) nil nil nil)
(print "clBuildProgram, errcode-ret = ")
(println (nth errcode-ret 0))

(def kernel (org.jocl.CL/clCreateKernel program "cv_rv" errcode-ret))
(print "clCreateKernel, errcode-ret = ")
(println (nth errcode-ret 0))

(let [{cv :cv rv :rv prod :prod} (prepare-mem context)]
  (def cv-mem cv)
  (def rv-mem rv)
  (def prod-mem prod))

(print "clSetKernalArg for arg 0, errcode-ret = ")
(println
  (org.jocl.CL/clSetKernelArg
   kernel 0 org.jocl.Sizeof/cl_mem (org.jocl.Pointer/to prod-mem))
   )
(print "clSetKernalArg for arg 1, errcode-ret = ")
(println
  (org.jocl.CL/clSetKernelArg
   kernel 1 org.jocl.Sizeof/cl_mem (org.jocl.Pointer/to cv-mem))
   )
(print "clSetKernalArg for arg 2, errcode-ret = ")
(println
  (org.jocl.CL/clSetKernelArg
   kernel 2 org.jocl.Sizeof/cl_mem (org.jocl.Pointer/to rv-mem))
   )
(print "clSetKernalArg for arg 3, errcode-ret = ")
(println
  (org.jocl.CL/clSetKernelArg
   kernel 3 org.jocl.Sizeof/cl_int (org.jocl.Pointer/to (int-array [N])))
   )

(print "clEnqueueWriteBuffer for cv-mem, errcode-ret = ")
(println
  (org.jocl.CL/clEnqueueWriteBuffer
   queue cv-mem org.jocl.CL/CL_TRUE
   0 (* 4 N) (org.jocl.Pointer/to cv-array)
   0 nil nil)
   )
(print "clEnqueueWriteBuffer for rv-mem, errcode-ret = ")
(println
  (org.jocl.CL/clEnqueueWriteBuffer
   queue rv-mem org.jocl.CL/CL_TRUE
   0 (* 4 N) (org.jocl.Pointer/to rv-array)
   0 nil nil)
   )

(print "clEnqueueNDRangeKernel, errcode-ret = ")
(let [kernel-done (make-array org.jocl.cl_event 1)]
  (println
    (org.jocl.CL/clEnqueueNDRangeKernel
     queue kernel 2 nil (long-array [4 4]) (long-array [1 1])
     ;queue kernel 1 nil (long-array [16]) (long-array [1])
     ;queue kernel 1 nil (long-array [4]) (long-array [1])
     0 nil nil))
     )

(def prod-array (float-array (* N N)))


(org.jocl.CL/clEnqueueReadBuffer
 queue prod-mem org.jocl.CL/CL_TRUE
 0 (* 4 N N) (org.jocl.Pointer/to prod-array)
 0 nil nil)

(pprint cv-array)
(pprint rv-array)
(pprint (partition 4 prod-array))

(def hoge-array (float-array N))
(org.jocl.CL/clEnqueueReadBuffer
 queue cv-mem org.jocl.CL/CL_TRUE
 0 (* 4 N) (org.jocl.Pointer/to hoge-array)
 0 nil nil)
(pprint hoge-array)

(org.jocl.CL/clFlush queue)
(org.jocl.CL/clFinish queue)
(org.jocl.CL/clReleaseKernel kernel)
(org.jocl.CL/clReleaseProgram program)
(org.jocl.CL/clReleaseMemObject cv-mem)
(org.jocl.CL/clReleaseMemObject rv-mem)
(org.jocl.CL/clReleaseMemObject prod-mem)
(org.jocl.CL/clReleaseCommandQueue queue)
(org.jocl.CL/clReleaseContext context);
