#include <ctime>
#include <sys/time.h>
#include <cstdarg>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <omp.h>

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
  vector< vector<double> > A;
  A.resize(n1);
  for(i=0; i < n1; ++i) {
    A[i].resize(n2);
    for(j=0; j < n2; ++j)
      A[i][j] = j+1.0;
  }
  printTime("Initializing %d by %d matrix", n2, n3);
  vector< vector<double> > B;
  B.resize(n2);
  for(i=0; i < n2; ++i) {
    B[i].resize(n3);
    for(j=0; j < n3; ++j) B[i][j] = 1.0/(i+1.);
  }
  
  printTime("Performing a naive 4 omp-threaded (%d x %d) by (%d x %d) matrix multiplication", n1, n2, n2, n3);
  vector< vector<double> > C;
  C.resize(n1);
  for(i=0; i<n1; ++i){
    C[i].resize(n3,0);
  }
  double sum = 0;

omp_set_num_threads(4);
#pragma omp parallel for private(i,j,k) schedule(dynamic)
for(i=0; i < n1; ++i) {
  for(j=0; j < n3; ++j) {
    for(k=0; k < n2; ++k)
      C[i][j] += ( A[i][k] * B[k][j] );
#pragma omp critical
     sum += C[i][j];
     //      printf("%f, %d, %d, %d \n", sum, i,j,k);
  }
 }


 // sum = 0;
 //  for(vector<vector<double> >::iterator it = C.begin(); it != C.end(); ++it){
 //    for(std::vector<double>::iterator jt = it->begin(); jt != it->end(); ++jt){
 //    sum += *jt;
 //    }
 //  }

  
printTime("Finished performing a naive omp-threaded matrix multiplication. Sum = %lg", sum);
 
  return 0;
}


