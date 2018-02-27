#include <ctime>
#include <sys/time.h>
#include <cstdarg>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <pthread.h>

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



class mult_thread_args {
public:
vector< vector<double> >* pA;
vector< vector<double> >* pB;
vector< vector<double> >* pC;
int32_t from_n1;
int32_t to_n1;
int32_t from_n3;
int32_t to_n3;
mult_thread_args(vector< vector<double> >* _pA,
vector< vector<double> >* _pB,
vector< vector<double> >* _pC, int32_t _from_n1, int32_t _to_n1, int32_t _from_n3, int32_t _to_n3) : pA(_pA), pB(_pB), pC(_pC),
from_n1(_from_n1), to_n1(_to_n1), from_n3(_from_n3), to_n3(_to_n3) {}
};

void* mult_thread(void* args) {
mult_thread_args* mt_args = (mult_thread_args*)args;
const vector< vector<double> >& A = *mt_args->pA;
const vector< vector<double> >& B = *mt_args->pB;
vector< vector<double> >& C = *mt_args->pC;
int32_t i, j, k;
int32_t n2 = (int32_t)B.size();
for(i=mt_args->from_n1; i < mt_args->to_n1; ++i) {
  for(j=mt_args->from_n3; j < mt_args->to_n3; ++j) {
    for(k=0; k < n2; ++k){
      C[i][j] += ( A[i][k] * B[k][j] );
    }
  }
}
return NULL;
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
  printTime("Performing a naive 4-thread matrix multiplication");
  vector< vector<double> > C;
  C.resize(n1);
  for(i=0; i<n1; ++i){
    C[i].resize(n3,0);
  }
  
  double sum = 0;

  
  pthread_t thread[4];
  mult_thread_args args1(&A, &B, &C, 0, n1/2, 0, n3/2);
  if ( pthread_create(&thread[0], NULL, mult_thread, &args1) != 0 ) perror("Can't create");
  mult_thread_args args2(&A, &B, &C, 0, n1/2, n3/2, n3);
  if ( pthread_create(&thread[1], NULL, mult_thread, &args2) != 0 ) perror("Can't create");
  mult_thread_args args3(&A, &B, &C, n1/2, n1, 0, n3/2);
  if ( pthread_create(&thread[2], NULL, mult_thread, &args3) != 0 ) perror("Can't create");
  mult_thread_args args4(&A, &B, &C, n1/2, n1, n3/2, n3);
  if ( pthread_create(&thread[3], NULL, mult_thread, &args4) != 0 ) perror("Can't create");
  for(i=0; i < 4; ++i) {
    pthread_join(thread[i], NULL);
  }

  for(vector<vector<double> >::iterator it = C.begin(); it != C.end(); ++it){
    for(std::vector<double>::iterator jt = it->begin(); jt != it->end(); ++jt){
    sum += *jt;
    }
  }


  printTime("Finished a naive 4-thread matrix multiplication. Sum = %lg", sum);
  return 0;
}

