__kernel void fw1(
 __global const float *wm,
 __global const float *bv,
 __global const float *niv,
 __global float *nov,
 int n_niv) {
  unsigned int i = get_global_id(0);
  unsigned int j;
  float acc;

  acc = bv[i];
  for (j = 0; j < n_niv; j++) {
    acc += wm[i*n_niv+j]*niv[j];
  }
  nov[i] = 1.0 / (1.0 + exp(-acc));
}

__kernel void bw_w_1st(
 __global const float *niv,
 __global const float *psv,
 __global float *wam,
 int n_niv) {
  unsigned int i = get_global_id(1);
  unsigned int j = get_global_id(0);

  wam[i*n_niv+j] = niv[j]*psv[i];
}

__kernal void bw_w(
 __global const float *niv,
 __global const float *psv,
 __global float *wam,
 int n_niv) {
  unsigned int i = get_global_id(1);
  unsigned int j = get_global_id(0);

  wam[i*n_niv+j] += niv[j]*psv[i];
}

__kernel void bw_b(
 __global const float *psv,
 __global float *bav) {
  unsigned int i = get_global_id(0);

  bav[i] += psv[i];
}

__kernel void bw_psv(
 __global const float *wm,
 __global const float *psv_in, 
 __global const float *niv,
 __global float *psv_out,
 __local float *tmp,
 int width, int height) {
  int i, j;
  for (j = 0; j < width; j++) {
    tmp[j] = psv_in[0]*wm[j];
  }
  for (i = 0; i < height; i++) {
    for (j = 0; j < width; j++) {
      tmp[j] += psv_in[i]*wm[i*width+j];
    }
  }
  for (j = 0; j < width; j++) {
    psv_out[j] = tmp[j]*psv_in[j]*(1.0-psv_in[j]);
  }
}

__kernel void bw_1st(
 __global const float *eov,
 __global const float *nov,
 __global float *psv,
 float speed) {
  unsigned int i = get_global_id(0);
  psv[i] = (eov[i] - nov[i])*speed;
}

__kernel acc_w(
 __global float *addend,
 __global float *adder,
 float decay) {
  unsigned int i = get_global_id(0);
  addend[i] = addend[i]*decay + adder[i];
}

__kernel acc_diff_square(
 __global const float *x,
 __global const float *y,
 __global float *acc,
 int n) {
  int i;
  float diff;
  for (i = 0; i < n; i++) {
    diff = x[0] - y[0];
    *acc += diff*diff;
  }
}

__kernel set0(
 float *x) {
  unsigned int i = get_global_id(0);
  x[i] = 0.0;
}
