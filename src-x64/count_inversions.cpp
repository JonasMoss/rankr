#include <Rcpp.h>
#include "count_inversions.h"

// Make count_inversions templated, just as
// https://www.cplusplus.com/reference/algorithm/sort/
// modify the other functions too; use ranges / iterators.
// Approx. time: 8 hours.
// template <class RandomAccessIterator, class Compare>
// unsigned long long inversions(RandomAccessIterator first, RandomAccessIterator last, Compare comp);

const int insertion_threshold = 15;

unsigned long long insertion_sort(double* x, unsigned long long length) {

  unsigned long long swap_count = 0;

  if(length < 2) return 0;

  unsigned long long max_j = length - 1;

  for(unsigned long long i = length - 2; i < length; --i) {

    unsigned long long j = i;
    double val = x[i];

    for(; j < max_j && x[j + 1] < val; ++j) x[j] = x[j + 1];

    x[j] = val;
    swap_count += (j - i);

  }

  return swap_count;

}

unsigned long long merge(double* from, double* to, unsigned long long middle, unsigned long long length) {

  double* left = from;
  double* right = from + middle;

  unsigned long long swaps = 0;
  unsigned long long index = 0;
  unsigned long long right_length = length - middle;
  unsigned long long left_length = middle;

  while(left_length && right_length) {

    if(right[0] < left[0]) {

      to[index] = right[0];
      swaps += left_length;
      right_length--;
      right++;

    } else {

      to[index] = left[0];
      left_length--;
      left++;

    }

    index++;

  }

  if(left_length) {

    memcpy(to + index, left, left_length * sizeof(double));

  } else if(right_length) {

    memcpy(to + index, right, right_length * sizeof(double));

  }

  return swaps;

}

unsigned long long merge_sort(double* x, double* buffer, unsigned long long length) {

  unsigned long long swaps = 0;
  unsigned long long half;

  if(length < insertion_threshold) return insertion_sort(x, length);

  half = length / 2;
  swaps += merge_sort(x, buffer, half);
  swaps += merge_sort(x + half, buffer + half, length - half);
  swaps += merge(x, buffer, half, length);

  memcpy(x, buffer, length * sizeof(double));

  return swaps;

}

// [[Rcpp::export]]
unsigned long long count_inversions(std::vector<double> &x) {

  unsigned long long length = x.size();
  double* array_x = &x[0];
  double* array_y = new double[length];
  return merge_sort(array_x, array_y, length);

}
