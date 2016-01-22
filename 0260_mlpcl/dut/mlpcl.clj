(ns mlpcl)

(import '(org.jocl CL Sizeof Pointer cl_device_id cl_event))

(defn handle-cl-error [err-code]
  (when (not= err-code CL/CL_SUCCESS)
    (throw (Exception. (CL/stringFor_errorCode err-code)))))

(defn lcg [seed]
  (let [val (mod (+ (* seed 1103515245) 12345)
                 (bit-shift-left 1 32))]
    (cons val (lazy-seq (lcg val)))))

(defn find-platform-device []
  (->> (cl/getPlatformIDs)
       (mapcat #(vector % (cl/getDeviceIDs %)))
       (filter (fn [[_ d]]
                 (->> (cl/getDeviceInfo d 'CL_DEVICE_TYPE)
                      parse-device-type
                      (some #(= % 'CL_DEVICE_CPU)))))
       first))

(defn mem-ro [context N]
  (let [err (int-array 1)
        mem (CL/clCreateBuffer context CL/CL_MEM_READ_ONLY N nil err)
        _ (handle-cl-error (first err)]
    mem))

(defn mem-rw [context N]
  (let [err (int-array 1)
        mem (CL/clCreateBuffer context CL/CL_MEM_READ_WRITE N nil err)
        _ (handle-cl-error (first err)]
    mem))

(defn mlp-cl-context
 ([n-neuron-vector] (mlp-cl-context n-neuron-vector (repeat 0.0)))
 ([n-neuron-vector seq]
  (let [[p d] (find-platform-device)
        ctx (cl/clCreateContext d)
        q (cl/clCreateCommandQueue ctx d)
        ws (map (fn [[w h]] (mem-rw ctx (* Sizeof/cl_float w h)))
                (partition 2 1 n-neuron-vector))
        bs (map #(mem-rw ctx (* Sizeof/cl_float %))
                (next n-neuron-vector))
        novs (map #(mem-rw ctx (* Sizeof/cl_float %))
                  (next n-neuron-vector))
        was (map (fn [[w h]] (mem-rw ctx (* Sizeof/cl_float w h)))
                 (partition 2 1 n-neuron-vector))
        bas (map #(mem-rw ctx (* Sizeof/cl_float %))
                 (next n-neuron-vector))
