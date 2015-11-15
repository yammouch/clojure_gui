(ns cl)

; wrapper of OpenCL API

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
