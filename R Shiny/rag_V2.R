library(readr)
library(officer)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)
library(httr2)
library(jsonlite)

message("Loaded DOCX-ONLY rag_V2.R")

# ---------------------------
# Read DOCX only
# ---------------------------
read_docx_file <- function(path) {
  doc <- read_docx(path)
  s <- docx_summary(doc)
  
  tibble(
    source = basename(path),
    text = s %>%
      dplyr::filter(content_type == "paragraph") %>%
      dplyr::pull(text) %>%
      paste(collapse = "\n")
  )
}

# ---------------------------
# Chunking
# ---------------------------
chunk_text <- function(text, chunk_size = 700, overlap = 120) {
  text <- str_squish(text)
  
  if (nchar(text) <= chunk_size) return(text)
  
  starts <- seq(1, nchar(text), by = chunk_size - overlap)
  map_chr(starts, ~ str_sub(text, .x, .x + chunk_size - 1))
}

# ---------------------------
# Embeddings
# ---------------------------
hf_embed <- function(text, hf_token,
                     model = "sentence-transformers/all-MiniLM-L6-v2") {
  
  url <- paste0(
    "https://router.huggingface.co/hf-inference/models/",
    model,
    "/pipeline/feature-extraction"
  )
  
  resp <- request(url) %>%
    req_headers(
      Authorization = paste("Bearer", hf_token),
      `Content-Type` = "application/json"
    ) %>%
    req_body_json(list(inputs = list(text))) %>%
    req_perform()
  
  out <- resp_body_json(resp, simplifyVector = TRUE)
  
  as.numeric(out[[1]])
}

cosine_sim <- function(a, b) {
  sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))
}

# ---------------------------
# Resolve DOCX path
# ---------------------------
resolve_docx_path <- function(file_path = NULL) {
  if (!is.null(file_path)) {
    if (!file.exists(file_path)) {
      stop(paste("DOCX file was not found:", file_path))
    }
    return(file_path)
  }
  
  candidate_paths <- c(
    "../RAG/Final Project RAG Document.docx"
    # "Final Project RAG Document.docx"
  )
  
  existing <- candidate_paths[file.exists(candidate_paths)]
  
  if (length(existing) == 0) {
    stop(
      paste(
        "Final Project RAG Document.docx was not found in any expected location.",
        "Checked:",
        paste(candidate_paths, collapse = "\n"),
        sep = "\n"
      )
    )
  }
  
  existing[1]
}

# ---------------------------
# Build local chunk store
# ---------------------------
build_chunk_store <- function(file_path, hf_token,
                              chunk_size = 700, overlap = 120) {
  
  docs <- read_docx_file(file_path)
  
  chunks <- docs %>%
    mutate(chunk = map(text, chunk_text, chunk_size = chunk_size, overlap = overlap)) %>%
    tidyr::unnest(chunk) %>%
    mutate(chunk_id = row_number()) %>%
    select(chunk_id, source, chunk)
  
  chunks$embedding <- purrr::map(chunks$chunk, hf_embed, hf_token = hf_token)
  
  chunks
}

initialize_rag_v2 <- function(
    store_path = "chunk_store.rds",
    file_path = NULL,
    hf_token = Sys.getenv("HUGGINGFACE_API_KEY"),
    force_rebuild = FALSE
) {
  
  message("Running initialize_rag_v2()")
  print(getwd())
  
  if (hf_token == "") {
    stop("HUGGINGFACE_API_KEY is not set")
  }
  
  resolved_docx <- resolve_docx_path(file_path)
  print(resolved_docx)
  
  if (file.exists(store_path) && !force_rebuild) {
    return(readRDS(store_path))
  }
  
  store <- build_chunk_store(
    file_path = resolved_docx,
    hf_token = hf_token,
    chunk_size = 700,
    overlap = 120
  )
  
  saveRDS(store, store_path)
  store
}

# ---------------------------
# Retrieval
# ---------------------------
retrieve_chunks <- function(question, store, hf_token, k = 3) {
  q_emb <- hf_embed(question, hf_token)
  
  sims <- purrr::map_dbl(store$embedding, ~ cosine_sim(q_emb, .x))
  
  store %>%
    mutate(score = sims) %>%
    arrange(desc(score)) %>%
    slice_head(n = k)
}

# ---------------------------
# Dashboard context
# ---------------------------
format_dashboard_context <- function(context = NULL) {
  if (is.null(context)) return("")
  
  paste(
    "Current dashboard filters:",
    paste("Months:", paste(context$months, collapse = ", ")),
    paste("Pickup weekdays:", paste(context$weekdays, collapse = ", ")),
    paste("Filtered rows:", context$n_rows),
    sep = "\n"
  )
}

# ---------------------------
# Hugging Face chat call
# ---------------------------
hf_chat <- function(question, retrieved, hf_token,
                    dashboard_context = "",
                    model = "meta-llama/Llama-3.1-8B-Instruct:cerebras") {
  
  context <- paste(
    paste0("Chunk ", retrieved$chunk_id, " from ", retrieved$source, ":\n", retrieved$chunk),
    collapse = "\n\n---\n\n"
  )
  
  prompt <- paste(
    "Answer the question only using the context below.",
    "If the answer is not in the context, say you do not know.",
    "Keep the answer short and clear.",
    "",
    if (dashboard_context != "") paste("Dashboard context:\n", dashboard_context) else "",
    "",
    "Context:",
    context,
    "",
    "Question:",
    question,
    sep = "\n"
  )
  
  resp <- request("https://router.huggingface.co/v1/chat/completions") %>%
    req_headers(
      Authorization = paste("Bearer", hf_token),
      `Content-Type` = "application/json"
    ) %>%
    req_body_json(list(
      model = model,
      messages = list(
        list(
          role = "system",
          content = "You are a helpful assistant answering questions about a student data science project."
        ),
        list(
          role = "user",
          content = prompt
        )
      ),
      temperature = 0.2,
      max_tokens = 250
    )) %>%
    req_perform()
  
  out <- resp_body_json(resp, simplifyVector = FALSE)
  
  if (!is.null(out$choices) &&
      length(out$choices) > 0 &&
      !is.null(out$choices[[1]]$message$content)) {
    return(out$choices[[1]]$message$content)
  }
  
  return("No answer was returned.")
}

# ---------------------------
# Main function for app.R
# ---------------------------
run_rag <- function(question, context = NULL, store,
                    hf_token = Sys.getenv("HUGGINGFACE_API_KEY"),
                    k = 3,
                    debug = FALSE) {
  
  if (hf_token == "") {
    stop("HUGGINGFACE_API_KEY is not set")
  }
  
  dashboard_context <- format_dashboard_context(context)
  
  retrieved <- retrieve_chunks(
    question = question,
    store = store,
    hf_token = hf_token,
    k = k
  )
  
  if (isTRUE(debug)) {
    print(retrieved %>% select(chunk_id, source, score, chunk))
  }
  
  answer <- hf_chat(
    question = question,
    retrieved = retrieved,
    hf_token = hf_token,
    dashboard_context = dashboard_context
  )
  
  list(
    answer = answer,
    sources = retrieved
  )
}