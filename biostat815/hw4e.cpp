#include <ctime>
#include <sys/time.h>
#include <cstdarg>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <Eigen/Dense>


using Eigen::MatrixXd;

void printTime(const char* msg, ...) { // advanced syntax to allow variable
  va_list ap; // number of function arguments
  va_start(ap, msg);
  char buff[255];
  struct timeval tv;
  gettimeofday(&tv, NULL);
  time_t current_time = tv.tv_sec;
  strftime(buff, 120, "%Y/%m/%d %H:%M:%S", localtime(&current_time));
  fprintf(stderr,"[%s.%06d]\t", buff, tv.tv_usec);
  vfprintf(stderr, msg, ap);
  fprintf(stderr,"\n");
  va_end(ap);
}

int main(int argc, char** argv) {
  int32_t n1, n2, n3;
int32_t i, j, k;
n1 = atoi(argv[1]); n2 = atoi(argv[2]); n3 = atoi(argv[3]);
printTime("Performing a single threaded (%d x %d) by (%d x %d) Eigen matrix multiplication", n1, n2, n2, n3);
MatrixXd eA(n1, n2);
for(i=0; i < n1; ++i) {
for(j=0; j < n2; ++j)
eA(i,j) = j+1.;
}
MatrixXd eB(n2, n3);
for(i=0; i < n2; ++i) {
for(j=0; j < n3; ++j)
eB(i,j) = 1./(i+1);
}
MatrixXd eC = eA * eB;
printTime("Finished performing a single-threaded Eigen multiplication. Sum = %lg", eC.sum());
  return 0;
}

