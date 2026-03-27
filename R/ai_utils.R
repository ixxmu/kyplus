#' Call ChatGPT-Compatible API
#'
#' @description
#' A general-purpose function to call the OpenAI ChatGPT API (or any
#' compatible API). This is the shared base layer used by all AI-powered
#' functions in kyplus.
#'
#' @param system_prompt Character. The system prompt that sets the AI's role.
#' @param user_prompt Character. The user message/question to send.
#' @param model Character. Model name. Default is "gpt-4o".
#' @param api_key Character. API key. Default reads from environment
#'   variable \code{OPENAI_API_KEY}.
#' @param base_url Character. API base URL. Default reads from environment
#'   variable \code{OPENAI_BASE_URL} (set by \code{ai_setup}), falling back
#'   to \code{"https://api.openai.com/v1"}.
#' @param temperature Numeric. Sampling temperature (0-2). Default is 0.1.
#' @param max_tokens Integer or NULL. Maximum tokens in response.
#'   Default is NULL (model decides).
#' @param timeout_seconds Numeric. Request timeout in seconds. Default is 120.
#' @param parse_json Logical. If TRUE, attempts to parse the response content
#'   as JSON and returns a list/data.frame. If FALSE, returns raw text.
#'   Default is FALSE.
#' @param verbose Logical. Print progress messages. Default is TRUE.
#'
#' @return If \code{parse_json = FALSE}, returns a list with:
#'   \describe{
#'     \item{content}{Character. The AI response text.}
#'     \item{usage}{List with token usage (prompt_tokens, completion_tokens,
#'       total_tokens). NULL if not available.}
#'     \item{model}{Character. The model used.}
#'   }
#'   If \code{parse_json = TRUE}, the \code{content} field will be the
#'   parsed JSON object (list or data.frame).
#'
#' @details
#' API key can be set via:
#' \code{Sys.setenv(OPENAI_API_KEY = "your-key-here")}
#'
#' Supports any OpenAI-compatible API by changing \code{base_url}.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- ai_chat(
#'     system_prompt = "You are a helpful assistant.",
#'     user_prompt = "What is PCA in bioinformatics?"
#' )
#' cat(result$content)
#'
#' # Get JSON response
#' result <- ai_chat(
#'     system_prompt = "Return JSON only.",
#'     user_prompt = "List 3 common cell types in PBMC as JSON array.",
#'     parse_json = TRUE
#' )
#' str(result$content)
#'
#' # Custom endpoint
#' result <- ai_chat(
#'     system_prompt = "You are a helpful assistant.",
#'     user_prompt = "Hello!",
#'     base_url = "https://your-proxy.com/v1",
#'     model = "gpt-4o-mini"
#' )
#' }
#'
#' @export
ai_chat <- function(system_prompt,
                    user_prompt,
                    model = "gpt-4o",
                    api_key = Sys.getenv("OPENAI_API_KEY"),
                    base_url = .ai_get_base_url(),
                    temperature = 0.1,
                    max_tokens = NULL,
                    timeout_seconds = 120,
                    parse_json = FALSE,
                    verbose = TRUE) {
    # --- Check dependencies ---
    if (!requireNamespace("httr", quietly = TRUE)) {
        stop(
            "\u9700\u8981 httr \u5305\u3002\u8bf7\u5148\u5b89\u88c5: install.packages('httr')",
            call. = FALSE
        )
    }

    # --- Validate API key ---
    if (nchar(api_key) == 0) {
        stop(
            "\u672a\u627e\u5230 API key\u3002\u8bf7\u901a\u8fc7\u4ee5\u4e0b\u65b9\u5f0f\u8bbe\u7f6e:\n",
            "  Sys.setenv(OPENAI_API_KEY = 'your-api-key-here')\n",
            "\u6216\u901a\u8fc7 api_key \u53c2\u6570\u4f20\u5165\u3002",
            call. = FALSE
        )
    }

    # --- Build request body ---
    url <- paste0(gsub("/$", "", base_url), "/chat/completions")

    body <- list(
        model = model,
        messages = list(
            list(role = "system", content = system_prompt),
            list(role = "user", content = user_prompt)
        ),
        temperature = temperature
    )

    if (!is.null(max_tokens)) {
        body$max_tokens <- as.integer(max_tokens)
    }

    # --- Make API call ---
    if (verbose) {
        message("\u2192 \u6b63\u5728\u8c03\u7528 ", model, " ...")
    }

    response <- tryCatch(
        {
            httr::POST(
                url = url,
                httr::add_headers(
                    "Authorization" = paste("Bearer", api_key),
                    "Content-Type" = "application/json"
                ),
                body = jsonlite::toJSON(body, auto_unbox = TRUE),
                encode = "raw",
                httr::timeout(timeout_seconds)
            )
        },
        error = function(e) {
            stop("API \u8bf7\u6c42\u5931\u8d25: ", e$message, call. = FALSE)
        }
    )

    # --- Check HTTP status ---
    status <- httr::status_code(response)
    if (status != 200) {
        error_text <- httr::content(response, as = "text", encoding = "UTF-8")
        error_info <- tryCatch(
            jsonlite::fromJSON(error_text),
            error = function(e) list(error = list(message = error_text))
        )
        error_msg <- if (!is.null(error_info$error$message)) {
            error_info$error$message
        } else {
            error_text
        }
        stop("API \u8c03\u7528\u5931\u8d25 (HTTP ", status, "): ", error_msg,
            call. = FALSE
        )
    }

    # --- Parse response ---
    result_text <- httr::content(response, as = "text", encoding = "UTF-8")
    result_json <- jsonlite::fromJSON(result_text, simplifyVector = FALSE)

    content_text <- result_json$choices[[1]]$message$content
    usage <- result_json$usage

    if (verbose && !is.null(usage)) {
        message(
            "\u2192 Token \u7528\u91cf: ",
            "prompt=", usage$prompt_tokens,
            ", completion=", usage$completion_tokens,
            ", total=", usage$total_tokens
        )
    }

    # --- Parse JSON content if requested ---
    if (parse_json) {
        content_text <- .ai_extract_json(content_text)
    }

    return(list(
        content = content_text,
        usage = usage,
        model = model
    ))
}


#' Extract and Parse JSON from AI Response Text
#'
#' @description
#' Internal helper that strips markdown code fences and parses JSON from
#' AI response content. Handles various return formats robustly.
#'
#' @param text Character. Raw response text that may contain JSON.
#'
#' @return Parsed JSON as list or data.frame.
#'
#' @keywords internal
.ai_extract_json <- function(text) {
    text <- trimws(text)
    # Remove markdown code fences
    text <- gsub("^```json\\s*\n?", "", text)
    text <- gsub("^```\\s*\n?", "", text)
    text <- gsub("\n?```\\s*$", "", text)
    text <- trimws(text)

    tryCatch(
        jsonlite::fromJSON(text, simplifyDataFrame = TRUE),
        error = function(e) {
            stop("\u89e3\u6790 JSON \u5931\u8d25: ", e$message,
                "\n\u539f\u59cb\u6587\u672c:\n", text,
                call. = FALSE
            )
        }
    )
}


#' Set AI API Configuration
#'
#' @description
#' Convenience function to set OpenAI API key and optional base URL
#' for the current R session.
#'
#' @param api_key Character. Your API key.
#' @param base_url Character. Optional custom base URL for compatible APIs.
#'
#' @examples
#' \dontrun{
#' ai_setup(api_key = "sk-xxxx")
#' ai_setup(api_key = "sk-xxxx", base_url = "https://your-proxy.com/v1")
#' }
#'
#' @export
ai_setup <- function(api_key, base_url = NULL) {
    Sys.setenv(OPENAI_API_KEY = api_key)
    message("\u2713 API key \u5df2\u8bbe\u7f6e")

    if (!is.null(base_url)) {
        Sys.setenv(OPENAI_BASE_URL = base_url)
        message("\u2713 Base URL \u5df2\u8bbe\u7f6e: ", base_url)
    }

    invisible(TRUE)
}


#' Get AI API Base URL
#'
#' @description
#' Internal helper to resolve the API base URL. Reads from the
#' \code{OPENAI_BASE_URL} environment variable (set by \code{ai_setup}),
#' falling back to the default OpenAI API URL.
#'
#' @return Character. The resolved base URL.
#'
#' @keywords internal
.ai_get_base_url <- function() {
    url <- Sys.getenv("OPENAI_BASE_URL", unset = "")
    if (nchar(url) > 0) {
        return(url)
    }
    "https://api.openai.com/v1"
}
