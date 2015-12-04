; $ java -cp /path/to/JOCL-x.x.x.jar:/path/to/clojure-x.x.x.jar -Djava.library.path="/path/to/JOCL-x.x.x-bin" clojure.main -i clInfo.clj

; very thin wrapper of OpenCL API

(defn clGetPlatformIDs []
  (let [num-entries 256
        platforms (make-array org.jocl.cl_platform_id num-entries)
        num-platforms (int-array 1)]
    (org.jocl.CL/clGetPlatformIDs num-entries platforms num-platforms)
    (take (nth num-platforms 0) platforms)))

(defn clGetPlatformInfo [platform param-name]
  (let [param-value-size 65536
        errcode-ret (int-array 1)
        param-value-body (byte-array param-value-size)
        param-value (org.jocl.Pointer/to param-value-body)
        param-value-size-ret (long-array 1)]
    (org.jocl.CL/clGetPlatformInfo
     platform    
     (.get (.getField org.jocl.CL (str param-name)) nil)
     param-value-size
     param-value
     param-value-size-ret)
    (take (nth param-value-size-ret 0)
          param-value-body)))

(defn clGetDeviceIDs [platform]
  (let [num-devices (int-array 1)
        _ (org.jocl.CL/clGetDeviceIDs
           platform org.jocl.CL/CL_DEVICE_TYPE_ALL 0 nil num-devices)
        devices (make-array org.jocl.cl_device_id (nth num-devices 0))]
    (org.jocl.CL/clGetDeviceIDs
     platform org.jocl.CL/CL_DEVICE_TYPE_ALL (nth num-devices 0)
     devices num-devices)
    (seq devices)))

(defn clGetDeviceInfo [device param-name]
  (let [param-value-size 65536
        param-value-body (byte-array param-value-size)
        param-value (org.jocl.Pointer/to param-value-body)
        param-value-size-ret (long-array 1)]
    (org.jocl.CL/clGetDeviceInfo
     device
     (.get (.getField org.jocl.CL (str param-name)) nil)
     param-value-size
     param-value
     param-value-size-ret)
    (take (nth param-value-size-ret 0) param-value-body)
    ))

; subroutines for get bunch of OpenCL infomation

(def long-props (map #(symbol (str "CL_DEVICE_" %))
                '[VENDOR_ID
                  MAX_COMPUTE_UNITS
                  MAX_WORK_ITEM_DIMENSIONS
                  MAX_WORK_GROUP_SIZE
                  PREFERRED_VECTOR_WIDTH_CHAR
                  PREFERRED_VECTOR_WIDTH_SHORT
                  PREFERRED_VECTOR_WIDTH_INT
                  PREFERRED_VECTOR_WIDTH_FLOAT
                  PREFERRED_VECTOR_WIDTH_DOUBLE
                  MAX_CLOCK_FREQUENCY
                  ADDRESS_BITS
                  MAX_MEM_ALLOC_SIZE
                  IMAGE_SUPPORT
                  MAX_READ_IMAGE_ARGS
                  MAX_WRITE_IMAGE_ARGS
                  IMAGE2D_MAX_WIDTH
                  IMAGE2D_MAX_HEIGHT
                  IMAGE3D_MAX_WIDTH
                  IMAGE3D_MAX_HEIGHT
                  IMAGE3D_MAX_DEPTH
                  MAX_SAMPLERS
                  MAX_PARAMETER_SIZE
                  MEM_BASE_ADDR_ALIGN
                  MIN_DATA_TYPE_ALIGN_SIZE
                  GLOBAL_MEM_CACHELINE_SIZE
                  GLOBAL_MEM_CACHE_SIZE
                  GLOBAL_MEM_SIZE
                  MAX_CONSTANT_BUFFER_SIZE
                  MAX_CONSTANT_ARGS
                  LOCAL_MEM_SIZE
                  ERROR_CORRECTION_SUPPORT
                  PROFILING_TIMER_RESOLUTION
                  ENDIAN_LITTLE
                  AVAILABLE
                  COMPILER_AVAILABLE]))

(def str-props (map #(symbol (str "CL_DEVICE_" %))
               '[NAME
                 VENDOR
                 PROFILE
                 VERSION
                 EXTENSIONS]))

(def hex-props (map #(symbol (str "CL_DEVICE_" %))
               '[SINGLE_FP_CONFIG
                 QUEUE_PROPERTIES]))

(defn parse-unsigned-info [array]
  (reduce (fn [acc x] (+ (* 256 acc) x))
          (reverse (map (fn [x] (if (neg? x) (+ x 256) x))
                        array))))
(defn parse-str-info [array]
  (apply str (map char (butlast array))))
(defn parse-device-type [array]
  (let [types (map #(symbol (str "CL_DEVICE_TYPE_" %))
                   '[DEFAULT CPU GPU ACCELERATOR])
        type-vals (map #(.get (.getField org.jocl.CL (str %)) nil)
                       types)
        u (parse-unsigned-info array)]
    (vec (map first
              (remove #(= 0 (get % 1))
                      (map (fn [t tv] [t (bit-and u tv)]) types type-vals)
                      )))))
(defn parse-size-t-array [array]
  (vec (map parse-unsigned-info
            (partition org.jocl.Sizeof/size_t array)
            )))

(defn get-device-info [device]
  (let [long-info (map #(clGetDeviceInfo device %)
                       long-props)
        str-info (map #(clGetDeviceInfo device %)
                      str-props)
        hex-info (map #(clGetDeviceInfo device %)
                      hex-props)]
    (concat (map vector long-props (map parse-unsigned-info long-info))
            (map vector str-props (map parse-str-info str-info))
            (map vector hex-props (map parse-unsigned-info hex-info))
            [['CL_DEVICE_TYPE
              (parse-device-type (clGetDeviceInfo device 'CL_DEVICE_TYPE))]
             ['CL_DEVICE_MAX_WORK_ITEM_SIZES
              (parse-size-t-array
               (clGetDeviceInfo device 'CL_DEVICE_MAX_WORK_ITEM_SIZES))]]
               )))

(defn get-platform-info [platform]
  (let [names '[CL_PLATFORM_PROFILE
                CL_PLATFORM_VERSION
                CL_PLATFORM_NAME
                CL_PLATFORM_VENDOR
                CL_PLATFORM_EXTENSIONS]]
    (concat (map vector
             names
             (map #(parse-str-info (clGetPlatformInfo platform %))
                  names))
            (map get-device-info (clGetDeviceIDs platform))
            )))

(def platforms (clGetPlatformIDs))
(def platforms-info (map get-platform-info platforms))
(pprint platforms-info)
