#include <Rcpp.h>
#include <map>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP make_unique_cpp(SEXP names, SEXP sep, SEXP prefix) {
  R_xlen_t len = LENGTH(names);
  if (len == 0) return names;

  SEXP result = PROTECT(Rf_allocVector(STRSXP, len));

  // 复制原始向量
  for(R_xlen_t i = 0; i < len; i++) {
    SET_STRING_ELT(result, i, STRING_ELT(names, i));
  }

  std::map<std::string, int> name_count;

  // 处理每个名字
  for (R_xlen_t i = 0; i < len; i++) {
    const char* current = CHAR(STRING_ELT(names, i));
    std::string curr_str(current);

    if (name_count.find(curr_str) != name_count.end()) {
      // 如果这个名字之前出现过，添加后缀
      name_count[curr_str]++;
      std::string new_name = curr_str +
        CHAR(STRING_ELT(sep, 0)) +
        CHAR(STRING_ELT(prefix, 0)) +
        std::to_string(name_count[curr_str]);
      SET_STRING_ELT(result, i, Rf_mkChar(new_name.c_str()));
    } else {
      // 第一次出现，初始化计数器
      name_count[curr_str] = 0;
    }
  }

  UNPROTECT(1);
  return result;
}
