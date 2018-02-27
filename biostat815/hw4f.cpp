#include <ctime>
#include <sys/time.h>
#include <cstdarg>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <cblas.h>

using namespace std;

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
  n1 = atoi(argv[1]);
  n2 = atoi(argv[2]);
  n3 = atoi(argv[3]);
  printTime("Initializing %d by %d matrix", n1, n2);
double* aA = new double[n1*n2];
for(i=0; i < n1; ++i) {
for(j=0; j < n2; ++j)
aA[i + j * n1] = j+1.;
}
   printTime("Initializing %d by %d matrix", n2, n3);
double* aB = new double[n2*n3];
for(i=0; i < n2; ++i) {
for(j=0; j < n3; ++j)
aB[i + j * n2] = 1./(i+1);
}
  printTime("Performing a naive matrix multiplication");
 double* aC = new double[n1*n3];
cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, n1, n3, n2, 1.0, aA, n1, aB, n2, 0, aC, n1);
double sum = 0;
for(i=0; i < n1; ++i) {
for(j=0; j < n3; ++j)
sum += aC[i + j * n1];
}
delete[] aA; delete[] aB; delete[] aC;
  printTime("Finished a naive matrix multiplication. Sum = %lg", sum);
  return 0;
}

