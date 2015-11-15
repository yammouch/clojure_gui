__kernel void cv_rv(
 __global float *prod,
 __global float *cv,
 __global float *rv,
 int n) {
  unsigned int i = get_global_id(0);
  unsigned int j = get_global_id(1);

  prod[i*n+j] = cv[i]*rv[j];
}
