(require 'cl)

(defn lcg [seed]
  (let [val (mod (+ (* seed 1103515245) 12345)
                 (bit-shift-left 1 32))]
    (cons val (lazy-seq (lcg val)))
    ))

(def platform (first (cl/clGetPlatformIDs)))
(def devices (cl/clGetDeviceIDs (platform)))

(def errcode-ret (int-array 1))

;(defn clCreateContext [device]
;  (let [errcode-ret (int-array 1)]
;    (org.jocl.CL/clCreateContext nil 1 device nil nil errcode-ret)))
;
;(def context (clCreateContext (first devices)))

(def context (org.jocl.CL/clCreateContext nil 1 device nil nil errcode-ret))

;(defn clCreateCommandQueue [context device]
;  (let [errcode-ret (int-array 1)]
;    (clCreateCommandQueue context device nil errcode-ret)))
;
;(def queue (clCreateCommandQueue context (first device)))

(def queue (org.jocl.CL/clCreateCommandQueue context (first devices)
                                             nil errcode-ret))

(def N 4)

;(def cv (map #(/ % 1.0 (bit-shift-left 1 32)) (take N (lcg 1))))
(def cv (map #(- (mod % 19) 9) (take N (lcg 1))))
(def cv-array (float-array cv))
(def cv-mem (org.jocl.CL/clCreateBuffer
             context org.jocl.CL/CL_MEM_READ_WRITE
             (* N 4) ; 4 -> sizeof(float)
             ;(org.jocl.Pointer/to cv-array)
             nil
             errcode-ret))

(def rv (map #(/ % 1.0 (bit-shift-left 1 32)) (take N (lcg 2))))
(def rv-array (floar-array rv))
(def rv-mem (org.jocl.CL/clCreateBuffer
             context org.jocl.CL/CL_MEM_READ_WRITE
             (* N 4) ; 4 -> sizeof(float)
             ;(org.jocl.Pointer/to rv-array)
             nil
             errcode-ret))

(def prod-mem (org.jocl.CL/clCreateBuffer
               context org.jocl.CL/CL_MEM_READ_WRITE
               (* N N 4) ; 4 -> sizeof(float)
               nil errcode-ret))

(let [src (slurp "cv_rv.cl")]
  (def program (org.jocl.CL/clCreateProgramWithSource
                context 1 src (count src) errcode-ret)))

(org.jocl.CL/clBuildProgram program 1 (first devices) nil nil nil)

(def kernel (org.jocl.CL/clCreateKernel program "cv_rv" errcode-ret))

(org.jocl.CL/clSetKernelArg kernel 0 (* 4 N N) prod-mem)
(org.jocl.CL/clSetKernelArg kernel 1 (* 4 N) cv-mem)
(org.jocl.CL/clSetKernelArg kernel 2 (* 4 N) rv-mem)
(org.jocl.CL/clSetKernelArg kernel 3 4 (int-array [N]))

(org.jocl.CL/clEnqueueWriteBuffer
 queue cv-mem org.jocl.CL/CL_TRUE
 0 (* 4 N) (org.jocl.Pointer/to cv-array)
 0 nil nil)
(org.jocl.CL/clEnqueueWriteBuffer
 queue rv-mem org.jocl.CL/CL_TRUE
 0 (* 4 N) (org.jocl.Pointer/to rv-array)
 0 nil nil)

(let [kernel-done (make-array org.jocl.CL.cl_event 1)
  (org.jocl.CL/clEnqueueNDRangeKernel
   queue kernel 2 (long-array [4 4]) (long-array [4 4]) (long-array [1 1])
   0 nil kernel-done))

(def prod-array (float-array (* 4 4)))

(org.jocl.CL/clEnqueueReadBuffer
 queue prod-mem org.jocl.CL/CL_TRUE
 0 (* 4 N N) (org.jocl.Pointer/to prod-array)
 0 nil nil)

(print (partition 4 prod-array))

(org.jocl.CL/clFlush queue)
(org.jocl.CL/clFinish queue)
(org.jocl.CL/clReleaseKernel kernel)
(org.jocl.CL/clReleaseProgram program)
(org.jocl.CL/clReleaseMemObject cv-mem)
(org.jocl.CL/clReleaseMemObject rv-mem)
(org.jocl.CL/clReleaseMemObject prod-mem)
(org.jocl.CL/clReleaseCommandQueue queue)
(org.jocl.CL/clReleaseContext context);
