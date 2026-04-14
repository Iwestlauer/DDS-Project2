# install packages
# install.packages(c(
#   "httr2", "jsonlite", "dplyr", "purrr", "stringr",
#   "readr", "pdftools", "officer", "shiny", "tibble"
# ))

# read files
library(readr)
library(pdftools)
library(officer)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

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
    text = s |>
      dplyr::filter(content_type == "paragraph") |>
      dplyr::pull(text) |>
      paste(collapse = "\n")
  )
}

# chunking
chunk_text <- function(text, chunk_size = 700, overlap = 120) {
  text <- str_squish(text)
  
  if (nchar(text) <= chunk_size) return(text)
  
  starts <- seq(1, nchar(text), by = chunk_size - overlap)
  map_chr(starts, ~ str_sub(text, .x, .x + chunk_size - 1))
}

# hugging face embedding
library(httr2)
library(jsonlite)

hf_embed <- function(text, hf_token,
                     model = "sentence-transformers/all-MiniLM-L6-v2") {
  
  url <- paste0(
    "https://router.huggingface.co/hf-inference/models/",
    model,
    "/pipeline/feature-extraction"
  )
  
  resp <- request(url) |>
    req_headers(
      Authorization = paste("Bearer", hf_token),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(inputs = list(text))) |>
    req_perform()
  
  out <- resp_body_json(resp, simplifyVector = TRUE)
  
  as.numeric(out[[1]])
}

# build local chunk store
files <- list(
  read_rmd_file("Main RMDs/Project V1.Rmd"),
  # read_pdf_file("paper1.pdf"),
  read_pdf_file("Project Docs/data_dictionary_trip_records_yellow.pdf"),
  read_docx_file("RAG/Final Project RAG Document.docx")
)

docs <- bind_rows(files)

chunks <- docs |>
  mutate(chunk = map(text, chunk_text)) |>
  tidyr::unnest(chunk) |>
  mutate(chunk_id = row_number()) |>
  select(chunk_id, source, chunk)

hf_token <- Sys.getenv("HUGGINGFACE_API_KEY")

if (hf_token == "") {
  stop("HUGGINGFACE_API_KEY is not set")
} else {
  cat("HUGGINGFACE_API_KEY found\n")
}

chunks$embedding <- purrr::map(chunks$chunk, hf_embed, hf_token = hf_token)

saveRDS(chunks, "RAG/chunk_store.rds")

# retrieval
cosine_sim <- function(a, b) {
  sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))
}

retrieve_chunks <- function(question, store, hf_token, k = 3) {
  q_emb <- hf_embed(question, hf_token)
  
  sims <- purrr::map_dbl(store$embedding, ~ cosine_sim(q_emb, .x))
  
  store |>
    mutate(score = sims) |>
    arrange(desc(score)) |>
    slice_head(n = k)
}

# hugging face chat call
hf_chat <- function(question, retrieved, hf_token,
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
    "Context:",
    context,
    "",
    "Question:",
    question,
    sep = "\n"
  )
  
  resp <- request("https://router.huggingface.co/v1/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", hf_token),
      `Content-Type` = "application/json"
    ) |>
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
    )) |>
    req_perform()
  
  out <- resp_body_json(resp, simplifyVector = FALSE)
  
  if (!is.null(out$choices) &&
      length(out$choices) > 0 &&
      !is.null(out$choices[[1]]$message$content)) {
    return(out$choices[[1]]$message$content)
  }
  
  return(out)
}


