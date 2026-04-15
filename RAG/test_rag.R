# =========================
# test_rag.R
# Minimal RAG test with:
# - 1 DOCX file
# - Hugging Face embeddings
# - Hugging Face chat completion
# =========================

# ---- packages ----
needed <- c(
  "httr2", "jsonlite", "dplyr", "purrr", "stringr",
  "tibble", "officer"
)

to_install <- needed[!needed %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(officer)

# read files
library(readr)
library(pdftools)

# ---- settings ----
# docx_path <- "RAG/Final Project RAG Document.docx"
# pdf_path <- "Project Docs/data_dictionary_trip_records_yellow.pdf"
hf_token <- Sys.getenv("HUGGINGFACE_API_KEY")

embedding_model <- "sentence-transformers/all-MiniLM-L6-v2"
chat_model <- "meta-llama/Llama-3.1-8B-Instruct:cerebras"

# ---- checks ----
if (hf_token == "") stop("HUGGINGFACE_API_KEY is not set in your R environment.")
cat("HUGGINGFACE_API_KEY found\n")
# if (!file.exists(docx_path)) stop("DOCX file not found: ", docx_path)
# cat("DOCX file found\n")
# if (!file.exists(pdf_path)) stop("PDF file not found: ", pdf_path)

# ---- read docx ----
read_docx_text <- function(path) {
  doc <- read_docx(path)
  s <- docx_summary(doc)
  
  txt <- s %>%
    filter(content_type %in% c("paragraph", "table cell")) %>%
    pull(text) %>%
    paste(collapse = "\n")
  
  str_squish(txt)
}

read_rmd_file <- function(path) {
  tibble(
    source = basename(path),
    text = read_file(path)
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

read_pdf_file <- function(path) {
  txt <- pdf_text(path)
  tibble(
    source = basename(path),
    text = paste(txt, collapse = "\n")
  )
}
# 
# doc_text <- read_docx_text(docx_path)
# pdf_text <- read_pdf_file(pdf_path)

# cat("\nDocument length (characters): ", nchar(doc_text), "\n", sep = "")
# cat("\nFirst 500 characters:\n")
# cat(substr(doc_text, 1, 500), "\n\n")

# build local chunk store
files <- list(
  read_rmd_file("Main RMDs/Project V1.Rmd"),
  # read_pdf_file("paper1.pdf"),
  read_pdf_file("Project Docs/data_dictionary_trip_records_yellow.pdf"),
  read_docx_file("RAG/Final Project RAG Document.docx")
)

docs <- bind_rows(files)

# ---- chunking ----
chunk_text <- function(text, chunk_size = 700, overlap = 120) {
  text <- str_squish(text)
  
  if (nchar(text) <= chunk_size) return(text)
  
  starts <- seq(1, nchar(text), by = chunk_size - overlap)
  map_chr(starts, ~ str_sub(text, .x, .x + chunk_size - 1))
}

chunks <- docs |>
  mutate(chunk = map(text, chunk_text)) |>
  tidyr::unnest(chunk) |>
  mutate(chunk_id = row_number()) |>
  select(chunk_id, source, chunk)

cat("Number of chunks: ", nrow(chunks), "\n\n", sep = "")

# ---- embedding function ----
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
    req_body_json(list(
      inputs = text
    )) |>
    req_perform()
  
  out <- resp_body_json(resp, simplifyVector = TRUE)
  
  # flatten in case output is nested
  as.numeric(unlist(out))
}

# ---- quick embedding test ----
cat("Testing embedding endpoint...\n")
test_emb <- hf_embed("This is a short test sentence.", hf_token, embedding_model)
cat("Embedding length: ", length(test_emb), "\n\n", sep = "")

# ---- embed all chunks ----
cat("Embedding all chunks...\n")
chunks$embedding <- map(chunks$chunk, ~ hf_embed(.x, hf_token, embedding_model))
saveRDS(chunks, "RAG/chunk_store_test.rds")
cat("Saved chunk store to chunk_store_test.rds\n\n")

# ---- cosine similarity ----
cosine_sim <- function(a, b) {
  denom <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (denom == 0) return(0)
  sum(a * b) / denom
}

# ---- retrieval ----
retrieve_chunks <- function(question, store, hf_token, k = 3) {
  q_emb <- hf_embed(question, hf_token, embedding_model)
  
  sims <- map_dbl(store$embedding, ~ cosine_sim(q_emb, .x))
  
  store %>%
    mutate(score = sims) %>%
    arrange(desc(score)) %>%
    slice_head(n = k)
}

# ---- chat function ----
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

# ---- quick chat test ----
cat("Testing chat endpoint...\n")
chat_test <- request("https://router.huggingface.co/v1/chat/completions") |>
  req_headers(
    Authorization = paste("Bearer", hf_token),
    `Content-Type` = "application/json"
  ) |>
  req_body_json(list(
    model = chat_model,
    messages = list(
      list(role = "user", content = "Say hello in one short sentence.")
    ),
    temperature = 0.2,
    max_tokens = 30
  )) |>
  req_perform()

chat_test_out <- resp_body_json(chat_test, simplifyVector = FALSE)
cat("Chat test response:\n")
cat(chat_test_out$choices[[1]]$message$content, "\n\n")

# ---- ask a sample question ----
sample_question <- "What are three key insights?"
#What steps were taken for data cleaning?
#Were airport trips included?
#Describe the distribution of tip amount?
#How does tip amount and fare amount relate to each other?
#What was the final model?
#What is the meaning of airport_fee?

top_chunks <- retrieve_chunks(sample_question, chunks, hf_token, k = 3)

cat("Top retrieved chunks:\n")
print(top_chunks %>% select(chunk_id, source, score))
cat("\n")

final_answer <- hf_chat(sample_question, top_chunks, hf_token, chat_model)

cat("Final answer:\n")
cat(final_answer, "\n")