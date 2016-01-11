__kernel void fw1(
 __global const float *wm,
 __global const float *bv,
 __global const float *niv,
 __global float *nov,
 int n_niv
) {
  unsigned int i = get_global_id(0);
  unsigned int j;
  float acc;

  acc = bv[i];
  for (j = 0; j < n_niv; j++) {
    acc += wm[i*n_niv+j]*niv[j];
  }
  nov[i] = 1.0 / (1.0 + exp(-acc));
}

__kernal void bw_w (
 __global const float *niv,
 __global const float *psv,
 __global float *wam,
 int n_niv
) {
  unsigned int i = get_global_id(1);
  unsigned int j = get_global_id(0);

  wam[i*n_niv+j] += niv[j]*psv[i];
}

__kernel void bw_b (
 __global const float *psv,
 __global float *bav
) {
  unsigned int i = get_global_id(0);

  bav[i] += psv[i];
}
