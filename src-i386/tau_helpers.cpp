#include <Rcpp.h>

template<typename T>
unsigned long long sum_product(
    const std::vector<T> &x,
    const unsigned long long lower_index = 0,
    unsigned long long upper_index = 0) {

  if(upper_index == 0) upper_index = x.size();

  unsigned long result = 0;
  unsigned long tie_count = 0;

  for(unsigned long i = lower_index + 1; i < upper_index; i++) {

    if(x[i] == x[i - 1]) {

      tie_count++;

    } else if(tie_count > 0) {

      result += (tie_count * (tie_count + 1));
      tie_count = 0;

    }

  }

  if(tie_count > 0) result += (tie_count * (tie_count + 1));

  return result;

}

template<typename S, typename T>
double sum_product_xy(const std::vector<S> &x, const std::vector<T> &y) {

  double f_xy = 0;
  unsigned long long tie_count = 0;

  for(unsigned i = 1; i < x.size(); i++) {

    if(x[i] == x[i - 1]) {

      tie_count++;

    } else if (tie_count > 0) {

      f_xy += sum_product(y, i - tie_count - 1, i);
      tie_count = 0;

    }

  }

  if (tie_count > 0) f_xy += sum_product(y, x.size() - tie_count - 1);

  return f_xy;

}

template<typename S, typename T>
void lexicographic_sort(std::vector<S> &x, std::vector<T> &y) {

  std::vector<std::pair<S, T>> pairs(x.size());

  for(unsigned i = 0; i < x.size(); i++) pairs[i] = {x[i], y[i]};

  sort(pairs.begin(), pairs.end());

  unsigned long long i = 0;

  for (auto elem : pairs) {

    x[i] = elem.first;
    y[i] = elem.second;
    i++;

  }

}
