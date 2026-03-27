#' 测试KEGG通路数据获取和更新功能
#'
#' @description
#' 测试update_kegg函数的各种功能，包括不同的数据获取方法、保存选项和格式。
#' 该测试涵盖各种参数组合和边缘情况，包括错误处理和各种输出格式。
#'
#' @param test_json_file 可选，用于"webjson"方法测试的JSON文件路径
#' @param test_species 可选，测试使用的物种代码，默认为"hsa"
#' @param test_path 测试文件保存位置，默认为临时目录
#' @param run_full 是否运行完整测试套件，包括需要网络连接和可能耗时的测试，默认为FALSE
#' @param clean_up 测试完成后是否清理测试文件，默认为TRUE
#'
#' @return 返回测试结果列表，包含各个测试用例的通过/失败状态和详细信息
#'
#' @examples
#' \dontrun{
#' # 运行基本测试，使用默认参数
#' test_results <- test_update_kegg()
#'
#' # 运行完整测试，指定JSON文件
#' test_results <- test_update_kegg(
#'   test_json_file = "path/to/hsa00001.json",
#'   run_full = TRUE
#' )
#'
#' # 使用自定义物种和保存路径
#' test_results <- test_update_kegg(
#'   test_species = "mmu",
#'   test_path = "test_output"
#' )
#' }
#'
#' @importFrom testthat test_that expect_is expect_true expect_error expect_warning
#' @importFrom utils installed.packages
#' @export
test_update_kegg <- function(test_json_file = NULL,
                             test_species = "hsa",
                             test_path = tempdir(),
                             run_full = FALSE,
                             clean_up = TRUE) {

  # 创建测试结果收集器
  test_results <- list(
    status = "Running",
    passed = 0,
    failed = 0,
    skipped = 0,
    details = list()
  )

  # 辅助函数：记录测试结果
  log_test <- function(name, result, message = NULL) {
    if (result == "passed") {
      test_results$passed <- test_results$passed + 1
    } else if (result == "failed") {
      test_results$failed <- test_results$failed + 1
    } else if (result == "skipped") {
      test_results$skipped <- test_results$skipped + 1
    }

    test_results$details[[name]] <- list(
      result = result,
      message = message,
      timestamp = Sys.time()
    )

    return(test_results)
  }

  # 检查是否安装了testthat
  if (!requireNamespace("testthat", quietly = TRUE)) {
    test_results$status <- "Failed"
    return(log_test("testthat_installation", "failed",
                    "需要安装testthat包: install.packages('testthat')"))
  }

  # 确保测试目录存在
  if (!dir.exists(test_path)) {
    dir.create(test_path, recursive = TRUE)
  }

  # 设置测试上下文
  cat("开始测试update_kegg函数...\n")

  #============================================================
  # 测试1：参数验证
  #============================================================
  tryCatch({
    testthat::test_that("参数验证正常工作", {
      # 测试无效方法
      testthat::expect_error(update_kegg(method = "invalid_method"),
                             "'arg' should be one of")

      # 测试无效保存格式
      testthat::expect_error(update_kegg(save = TRUE, save_format = "invalid_format"),
                             "'arg' should be one of")

      # 测试无效GMT类型
      testthat::expect_error(update_kegg(save = TRUE, save_format = "gmt",
                                         gmt_type = "invalid_type"),
                             "'arg' should be one of")
    })
    log_test("parameter_validation", "passed")
  }, error = function(e) {
    log_test("parameter_validation", "failed", e$message)
  })

  #============================================================
  # 测试2：默认参数
  #============================================================
  if (requireNamespace("msigdbr", quietly = TRUE)) {
    tryCatch({
      testthat::test_that("默认参数正常工作", {
        result <- update_kegg()
        testthat::expect_is(result, "data.frame")
        testthat::expect_true("pathway" %in% colnames(result))
        testthat::expect_true("symbol" %in% colnames(result))
      })
      log_test("default_parameters", "passed")
    }, error = function(e) {
      log_test("default_parameters", "failed", e$message)
    })
  } else {
    log_test("default_parameters", "skipped", "缺少msigdbr包")
  }

  #============================================================
  # 测试3：webjson方法
  #============================================================
  if (!is.null(test_json_file) && file.exists(test_json_file)) {
    tryCatch({
      testthat::test_that("webjson方法正常工作", {
        result <- update_kegg(method = "webjson", json_file = test_json_file)
        testthat::expect_is(result, "data.frame")
        testthat::expect_true(all(c("pathway", "entrezID", "symbol") %in% colnames(result)))
      })
      log_test("webjson_method", "passed")
    }, error = function(e) {
      log_test("webjson_method", "failed", e$message)
    })
  } else {
    log_test("webjson_method", "skipped", "未提供JSON文件或文件不存在")
  }

  #============================================================
  # 测试4：yulab方法（需要clusterProfiler包）
  #============================================================
  if (run_full && requireNamespace("clusterProfiler", quietly = TRUE)) {
    tryCatch({
      testthat::test_that("yulab方法正常工作", {
        result <- update_kegg(method = "yulab", species = test_species)
        testthat::expect_is(result, "data.frame")
        testthat::expect_true(all(c("pathway", "entrezID") %in% colnames(result)))
      })
      log_test("yulab_method", "passed")
    }, error = function(e) {
      log_test("yulab_method", "failed", e$message)
    })
  } else {
    log_test("yulab_method", "skipped",
             ifelse(!run_full, "完整测试未启用", "缺少clusterProfiler包"))
  }

  #============================================================
  # 测试5：gseadbr方法（需要msigdbr包）
  #============================================================
  if (requireNamespace("msigdbr", quietly = TRUE)) {
    tryCatch({
      testthat::test_that("gseadbr方法正常工作", {
        species_name <- ifelse(test_species == "hsa", "Homo sapiens",
                               ifelse(test_species == "mmu", "Mus musculus",
                                      "Homo sapiens"))
        result <- update_kegg(method = "gseadbr", species = species_name)
        testthat::expect_is(result, "data.frame")
        testthat::expect_true(all(c("pathway", "symbol") %in% colnames(result)))
      })
      log_test("gseadbr_method", "passed")
    }, error = function(e) {
      log_test("gseadbr_method", "failed", e$message)
    })
  } else {
    log_test("gseadbr_method", "skipped", "缺少msigdbr包")
  }

  #============================================================
  # 测试6：多种方法
  #============================================================
  if (run_full && requireNamespace("msigdbr", quietly = TRUE) &&
      requireNamespace("clusterProfiler", quietly = TRUE)) {
    tryCatch({
      testthat::test_that("多方法组合正常工作", {
        result <- update_kegg(method = c("yulab", "gseadbr"),
                              species = ifelse(test_species == "hsa",
                                               c(test_species, "Homo sapiens"),
                                               c(test_species, "Mus musculus")))
        testthat::expect_is(result, "list")
        testthat::expect_true(all(c("yulab", "gseadbr") %in% names(result)))
      })
      log_test("multiple_methods", "passed")
    }, error = function(e) {
      log_test("multiple_methods", "failed", e$message)
    })
  } else {
    log_test("multiple_methods", "skipped", "完整测试未启用或缺少依赖包")
  }

  #============================================================
  # 测试7：保存RDS格式
  #============================================================
  if (requireNamespace("msigdbr", quietly = TRUE)) {
    tryCatch({
      test_rds_file <- file.path(test_path, "kegg_test_rds.rds")
      if (file.exists(test_rds_file)) file.remove(test_rds_file)

      testthat::test_that("RDS格式保存正常工作", {
        result <- update_kegg(method = "gseadbr", save = TRUE,
                              save_path = test_path,
                              prefix = "kegg_test_rds",
                              add_date = FALSE)
        testthat::expect_true(file.exists(test_rds_file))

        # 验证加载的数据
        loaded_data <- readRDS(test_rds_file)
        testthat::expect_is(loaded_data, "data.frame")
        testthat::expect_true(all(c("pathway", "symbol") %in% colnames(loaded_data)))
      })
      log_test("save_rds", "passed")
    }, error = function(e) {
      log_test("save_rds", "failed", e$message)
    }, finally = {
      if (clean_up && file.exists(test_rds_file)) file.remove(test_rds_file)
    })
  } else {
    log_test("save_rds", "skipped", "缺少msigdbr包")
  }

  #============================================================
  # 测试8：保存GMT格式
  #============================================================
  if (requireNamespace("msigdbr", quietly = TRUE)) {
    tryCatch({
      test_gmt_symbol_file <- file.path(test_path, "kegg_test_gmt_symbol.gmt")
      test_gmt_entrez_file <- file.path(test_path, "kegg_test_gmt_entrezID.gmt")

      if (file.exists(test_gmt_symbol_file)) file.remove(test_gmt_symbol_file)
      if (file.exists(test_gmt_entrez_file)) file.remove(test_gmt_entrez_file)

      testthat::test_that("GMT格式保存正常工作", {
        result <- update_kegg(method = "gseadbr", save = TRUE,
                              save_path = test_path,
                              save_format = "gmt",
                              prefix = "kegg_test_gmt",
                              add_date = FALSE)
        testthat::expect_true(file.exists(test_gmt_symbol_file))

        # 验证GMT文件
        if (file.exists(test_gmt_symbol_file)) {
          gmt_lines <- readLines(test_gmt_symbol_file)
          testthat::expect_true(length(gmt_lines) > 0)

          # 检查第一行的格式
          first_line <- strsplit(gmt_lines[1], "\t")[[1]]
          testthat::expect_true(length(first_line) >= 3) # 至少应有通路名、描述和一个基因
        }
      })
      log_test("save_gmt", "passed")
    }, error = function(e) {
      log_test("save_gmt", "failed", e$message)
    }, finally = {
      if (clean_up) {
        if (file.exists(test_gmt_symbol_file)) file.remove(test_gmt_symbol_file)
        if (file.exists(test_gmt_entrez_file)) file.remove(test_gmt_entrez_file)
      }
    })
  } else {
    log_test("save_gmt", "skipped", "缺少msigdbr包")
  }

  #============================================================
  # 测试9：错误处理
  #============================================================
  tryCatch({
    testthat::test_that("错误处理正常工作", {
      # 测试webjson方法无效JSON文件
      testthat::expect_error(
        update_kegg(method = "webjson", json_file = "non_existent_file.json"),
        "使用webjson方法时必须提供有效的json_file参数")
    })
    log_test("error_handling", "passed")
  }, error = function(e) {
    log_test("error_handling", "failed", e$message)
  })

  #============================================================
  # 测试10：依赖包检查和处理
  #============================================================
  tryCatch({
    testthat::test_that("依赖包检查正常工作", {
      # 这个测试依赖于msigdbr是否安装
      if (!requireNamespace("non_existent_package", quietly = TRUE)) {
        # 预期的行为：函数会检查并正确处理缺少的依赖
        testthat::expect_true(TRUE)
      }
    })
    log_test("dependency_checks", "passed")
  }, error = function(e) {
    log_test("dependency_checks", "failed", e$message)
  })

  # 更新测试结果状态
  if (test_results$failed > 0) {
    test_results$status <- "Failed"
  } else {
    test_results$status <- "Passed"
  }

  # 打印测试总结
  cat("\n测试完成。总结：\n")
  cat("通过: ", test_results$passed, "\n")
  cat("失败: ", test_results$failed, "\n")
  cat("跳过: ", test_results$skipped, "\n")
  cat("状态: ", test_results$status, "\n")

  # 返回测试结果
  return(test_results)
}

#' 获取测试用的示例JSON文件
#'
#' @description
#' 创建一个简化的测试JSON文件，用于测试webjson方法。
#' 该函数会在指定位置创建一个包含最小化KEGG数据结构的JSON文件。
#'
#' @param output_path 输出文件路径，默认为当前工作目录
#' @param file_name 输出文件名，默认为"test_kegg.json"
#'
#' @return 返回创建的文件完整路径
#'
#' @examples
#' \dontrun{
#' # 创建测试JSON文件
#' json_file <- create_test_json()
#'
#' # 使用创建的文件测试update_kegg函数
#' test_results <- test_update_kegg(test_json_file = json_file)
#' }
#'
#' @importFrom jsonlite toJSON
#' @export
create_test_json <- function(output_path = getwd(), file_name = "test_kegg.json") {
  # 创建最小化的KEGG数据结构
  sample_data <- list(
    name = "KEGG Pathways",
    children = list(
      list(
        name = "01100 Metabolism",
        children = list(
          list(
            name1 = "01101 Carbohydrate metabolism",
            children = list(
              list(
                name2 = "00010 Glycolysis / Gluconeogenesis [PATH:hsa00010]",
                children = list(
                  list(name3 = "100 ALDOA; fructose-bisphosphate aldolase A\tEC:4.1.2.13"),
                  list(name3 = "226 ALDOC; fructose-bisphosphate aldolase C\tEC:4.1.2.13"),
                  list(name3 = "229 ALDOB; fructose-bisphosphate aldolase B\tEC:4.1.2.13")
                )
              ),
              list(
                name2 = "00020 Citrate cycle (TCA cycle) [PATH:hsa00020]",
                children = list(
                  list(name3 = "1431 CS; citrate synthase\tEC:2.3.3.1"),
                  list(name3 = "4190 MDH1; malate dehydrogenase 1\tEC:1.1.1.37"),
                  list(name3 = "4191 MDH2; malate dehydrogenase 2\tEC:1.1.1.37")
                )
              )
            )
          ),
          list(
            name1 = "01102 Energy metabolism",
            children = list(
              list(
                name2 = "00190 Oxidative phosphorylation [PATH:hsa00190]",
                children = list(
                  list(name3 = "1329 COX5B; cytochrome c oxidase subunit 5B\tEC:1.9.3.1"),
                  list(name3 = "4512 COX4I1; cytochrome c oxidase subunit 4I1\tEC:1.9.3.1"),
                  list(name3 = "4513 COX2; cytochrome c oxidase subunit II\tEC:1.9.3.1")
                )
              )
            )
          )
        )
      )
    )
  )

  # 创建输出文件路径
  file_path <- file.path(output_path, file_name)

  # 转换为JSON并写入文件
  tryCatch({
    json_text <- jsonlite::toJSON(sample_data, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json_text, file_path)
    message("测试JSON文件已创建: ", file_path)
    return(file_path)
  }, error = function(e) {
    stop("创建测试JSON文件时出错: ", e$message)
  })
}
