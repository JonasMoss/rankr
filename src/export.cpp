#include <Rcpp.h>
#include "inversions.h"
#include "helpers.h"

// [[Rcpp::export]]
Rcpp::List tau_data(std::vector<double> &x, std::vector<double> &y)  {
  lexicographic_sort(x, y);
  double t_xy = sum_product_xy(x, y);
  unsigned long long inversions = count_inversions(y);
  double f_x = sum_product(x);
  double f_y = sum_product(y);
  double m = x.size() * (x.size() - 1);
  double n_d = 2 * inversions;
  double n_c = (m - f_x - f_y + t_xy) - 2 * inversions;
  return Rcpp::List::create(Rcpp::Named("n_c") = n_c,
                            Rcpp::Named("n_d") = n_d,
                            Rcpp::Named("t_x") = f_x - t_xy,
                            Rcpp::Named("t_y") = f_y - t_xy,
                            Rcpp::Named("t_xy") = t_xy,
                            Rcpp::Named("N") = m);
}

