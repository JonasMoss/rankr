#include <Rcpp.h>
#include "count_inversions.h"
#include "tau_helpers.h"

//' Estimate variants of Kendall's tau
//'
//' @param x,y Vectors of observations.
//' @return The variant of Kendall's tau.
//' @export
//' @name tau
// [[Rcpp::export]]
double tau_a(std::vector<double> x, std::vector<double> y) {

  lexicographic_sort(x, y);
  double f_xy = sum_product_xy(x, y);
  unsigned long long inversions = count_inversions(y);
  double f_x = sum_product(x);
  double f_y = sum_product(y);
  double m = x.size() * (x.size() - 1);

  double top = (m - f_x - f_y + f_xy) - 4 * inversions;
  double bottom = m;

  return top / bottom;

}

//' @rdname tau
//' @export
// [[Rcpp::export]]
double tau_b(std::vector<double> x, std::vector<double> y) {

  lexicographic_sort(x, y);

  double f_xy = sum_product_xy(x, y);
  unsigned long long inversions = count_inversions(y);
  double f_x = sum_product(x);
  double f_y = sum_product(y);
  double m = x.size() * (x.size() - 1);

  double top = (m - f_x - f_y + f_xy) - 4 * inversions;
  double bottom = pow((m - f_x) * (m - f_y), 0.5);

  return top / bottom;

}

//' @rdname tau
//' @export
// [[Rcpp::export]]
double tau_c(std::vector<double> x, std::vector<double> y) {

  lexicographic_sort(x, y);
  double f_xy = sum_product_xy(x, y);
  unsigned long long inversions = count_inversions(y);
  double f_x = sum_product(x);
  double f_y = sum_product(y);
  double m = x.size() * (x.size() - 1);

  double n = x.size();
  unsigned long long r = std::unique(x.begin(), x.end()) - x.begin();
  unsigned long long c = std::unique(y.begin(), y.end()) - y.begin();

  double top = (m - f_x - f_y + f_xy) - 4 * inversions;
  double bottom = n * n * (std::min(r, c) - 1) / std::min(r, c);

  return top / bottom;

}

//' @rdname tau
//' @export
// [[Rcpp::export]]
double tau_d(std::vector<double> x, std::vector<double> y) {

  lexicographic_sort(x, y); // lexicographically sorts x and y in-place.
  double f_xy = sum_product_xy(x, y); // assumes x,y are lexicographically sorted.
  unsigned long long inversions = count_inversions(y); // counts inversions and sorts y in place.
  double f_x = sum_product(x); // assumes x is sorted.
  double f_y = sum_product(y); // assumes y is sorted.
  double m = x.size() * (x.size() - 1);

  double top = (m - f_x - f_y + (2 * f_xy - f_y * f_x / m)) - 4 * inversions;
  double bottom = m - f_y + f_x - f_y * f_x / m;
  return top / bottom;

}

//' @rdname tau
//' @export
// [[Rcpp::export]]
double somers_d(std::vector<double> x, std::vector<double> y) {

  lexicographic_sort(x, y);
  double f_xy = sum_product_xy(x, y);
  unsigned long long inversions = count_inversions(y);
  double f_x = sum_product(x);
  double f_y = sum_product(y);
  double m = x.size() * (x.size() - 1);

  double top = (m - f_x - f_y + f_xy) - 4 * inversions;
  double bottom = (m - f_y);

  return top / bottom;

}

//' @rdname tau
//' @export
// [[Rcpp::export]]
double gk_gamma(std::vector<double> x, std::vector<double> y) {

  lexicographic_sort(x, y);
  double f_xy = sum_product_xy(x, y);
  unsigned long long inversions = count_inversions(y);
  double f_x = sum_product(x);
  double f_y = sum_product(y);
  double m = x.size() * (x.size() - 1);

  double top = (m - f_x - f_y + f_xy) - 4 * inversions;
  double bottom = (m - f_x - f_y + f_xy);

  return top / bottom;

}

// [[Rcpp::export]]
double sum_prod2(std::vector<double> &x, std::vector<double> &y) {
  lexicographic_sort(x, y);
  return sum_product_xy(x, y);
}
