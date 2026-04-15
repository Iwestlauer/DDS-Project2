library(readr)
library(pdftools)
library(officer)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)
library(httr2)
library(jsonlite)

message("Loaded NEW rag_V2.R")

read_rmd_file <- function(path) {
  tibble(
    source = basename(path),
    text = read_file(path)
  )
}

read_pdf_file <- function(path) {
  txt <- pdf_text(path)
  tibble(
    source = basename(path),
    text = paste(txt, collapse = "\n")
  )
}

read_docx_file <- function(path) {
  doc <- read_docx(path)
  s <- docx_summary(doc)
  
  tibble(
    source = basename(path),
    text = s %>%
      filter(content_type == "paragraph") %>%
      pull(text) %>%
      paste(collapse = "\n")
  )
}

read_project_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "rmd") return(read_rmd_file(path))
  if (ext == "pdf") return(read_pdf_file(path))
  if (ext == "docx") return(read_docx_file(path))
  
  stop(paste("Unsupported file type:", ext))
}

chunk_text <- function(text, chunk_size = 700, overlap = 120) {
  text <- str_squish(text)
  
  if (nchar(text) <= chunk_size) return(text)
  
  starts <- seq(1, nchar(text), by = chunk_size - overlap)
  map_chr(starts, ~ str_sub(text, .x, .x + chunk_size - 1))
}

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

build_chunk_store <- function(file_paths, hf_token,
                              chunk_size = 700, overlap = 120) {
  
  docs <- purrr::map_dfr(file_paths, read_project_file)
  
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
    file_paths = c(
      "Project V1.Rmd",
      "../Project Docs/data_dictionary_trip_records_yellow.pdf",
      "../RAG/Final Project RAG Document.docx"
    ),
    hf_token = Sys.getenv("HUGGINGFACE_API_KEY"),
    force_rebuild = FALSE
) {
  
  message("Running initialize_rag_v2()")
  print(getwd())
  print(file_paths)
  print(file.exists(file_paths))
  
  if (hf_token == "") {
    stop("HUGGINGFACE_API_KEY is not set")
  }
  
  missing_files <- file_paths[!file.exists(file_paths)]
  
  if (length(missing_files) > 0) {
    stop(
      paste(
        "The following RAG source files were not found:",
        paste(missing_files, collapse = "\n"),
        sep = "\n"
      )
    )
  }
  
  if (file.exists(store_path) && !force_rebuild) {
    return(readRDS(store_path))
  }
  
  store <- build_chunk_store(
    file_paths = file_paths,
    hf_token = hf_token
  )
  
  saveRDS(store, store_path)
  store
}

retrieve_chunks <- function(question, store, hf_token, k = 3) {
  q_emb <- hf_embed(question, hf_token)
  
  sims <- purrr::map_dbl(store$embedding, ~ cosine_sim(q_emb, .x))
  
  store %>%
    mutate(score = sims) %>%
    arrange(desc(score)) %>%
    slice_head(n = k)
}

format_dashboard_context <- function(context = NULL) {
  if (is.null(context)) return("")
  
  paste(
    "Current dashboard filters:",
    paste("Months:", paste(context$months, collapse = ", ")),
    paste("Pickup weekdays:", paste(context$weekdays, collapse = ", ")),
    paste("Source files:", paste(context$sources, collapse = ", ")),
    paste("Filtered rows:", context$n_rows),
    sep = "\n"
  )
}

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
    "Retrieved context:",
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

run_rag <- function(question, context = NULL, store,
                    hf_token = Sys.getenv("HUGGINGFACE_API_KEY"),
                    k = 3) {
  
  if (hf_token == "") {
    stop("HUGGINGFACE_API_KEY is not set")
  }
  
  dashboard_context <- format_dashboard_context(context)
  
  retrieval_query <- if (dashboard_context == "") {
    question
  } else {
    paste(question, dashboard_context)
  }
  
  retrieved <- retrieve_chunks(
    question = retrieval_query,
    store = store,
    hf_token = hf_token,
    k = k
  )
  
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
