__kernel void cv_rv(
#ifdef SIMD
 __global float4 *prod,
#else
 __global float *prod,
#endif
 __global float *cv,
#ifdef SIMD
 __global float4 *rv,
#else
 __global float *rv,
#endif
 int n) {
  unsigned int j = get_global_id(0);
  unsigned int i = get_global_id(1);

#ifdef SIMD
  prod[i*n+j] = (float4)(cv[i])*rv[j];
#else
  prod[i*n+j] = cv[i]*rv[j];
#endif
}

__kernel void cv_rv_loop(
 __global float *prod,
 __global float *cv,
 __global float *rv,
 int n) {
  unsigned int i, j;

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      prod[i*n+j] = cv[i]*rv[j];
}
