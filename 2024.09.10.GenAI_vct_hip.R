# Load necessary libraries
library(tidyverse)
library(future)
library(furrr)
#library(future.apply)
library(readxl)
library(rollama)
library(data.table)
#library(reticulate)
#library(tensorflow)
library(memoise)
# Set the environment variable to the desired Python executable
Sys.setenv(RETICULATE_PYTHON = "/home/kropinsp/miniconda3/bin/python")

#Get the ollama server details
ollama_server <- Sys.getenv("OLLAMA_HOST")
ollama_url <- paste("https://", ollama_server, sep="")
print(ollama_url)

run_genAI_vct_hip <- function(file_path, from = "2024-07-05", to = "2024-08-15", company_name="Sonic Healthcare") {
#  Sys.setlocale("LC_ALL", "English_United States.utf8")
  
  # Set up GPU environment
#  use_condaenv("C:/Users/pkrop/anaconda3/envs/ny_env", required = TRUE)
#  if (length(tf$config$list_physical_devices('GPU')) > 0) {
#    cat("GPU is available\n")
#  } else {
#    cat("GPU is not available\n") 
#  }
  options(rollama.gpu = TRUE); options(rollama.n_gpu = 1); options(rollama.main_gpu = 0)
  
  # Load and preprocess the data
  parOne <- read_excel(file_path)
  parOne <- parOne %>% 
    mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%S")) %>%
    filter(Timestamp >= as.POSIXct(from) & Timestamp <= as.POSIXct(to)) %>%
    arrange(desc(Timestamp))  # Sort by timestamp in descending order
  
  parOne_co <- company_name
  parOne_n <- nrow(parOne)
  
  # Set up a parallel plan with GPU
  plan(multisession, workers = availableCores() - 1)
  start_time <- Sys.time()  # Record the start time
  # Define a function to run the query with a specified model
  run_query <- function(model, q_th) {
    query(q_th, screen = FALSE, model = model, 
          model_params = list(seed = 42, temperature = 0, top_p = 0.8, top_k = 30))
  }
  
  #implement caching
  run_query_cached <- memoise(run_query)
  
  # Define a function to process each row
  # "Assume the role of a financial expert specializing in stock recommendations. When provided with news, analyze whether it is good news, bad news, or uncertain for stocks. Respond with ‘YES’ for good news, ‘NO’ for bad news, or ‘UNKNOWN’ for uncertain news, followed by a semicolon ‘;’. Then, provide a brief one-sentence explanation."
  #"For news, reply 'YES' (good), 'NO' (bad), or 'UNKNOWN' (uncertain) for stock impact; then a brief one-sentence explanation."
  process_row <- function(i) {
    q_th <- tribble(
      ~role, ~content, 
      "system", "Assume the role of a financial expert specializing in stock recommendations. When provided with news, analyze whether it is good news, bad news, or uncertain for stocks. Respond with ‘YES’ for good news, ‘NO’ for bad news, or ‘UNKNOWN’ for uncertain news, followed by a semicolon ‘;’. Then, provide a brief one-sentence explanation." ,
      "user", sprintf("Is this headline good or bad for the stock price of %s in the short term? Headline: %s", parOne_co, parOne$Column1[i])
    )
    
    models <- c("phi3:mini", "mistral", "llama3.1")
    results <- future_map(models, ~run_query(.x, q_th), .options = furrr_options(seed = TRUE))
    
    map(results, ~{
      result <- .x
      result$m_role <- result$message$role
      result$m_content <- result$message$content
      result$message <- NULL
      df_temp <- as.data.frame(t(as.data.frame(result, stringsAsFactors = FALSE)))
      df_temp <- as.data.frame(t(df_temp), stringsAsFactors = FALSE)
      cbind(parOne[i, , drop = FALSE], df_temp)
    })
  }
  
  # Process all rows in parallel
  all_results <- future_map(1:parOne_n, process_row, .options = furrr_options(seed = TRUE))
  
            # Record the end time          
            end_time <- Sys.time()  
            
            # Calculate the time difference in seconds
            elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
            
            # Convert elapsed time to hours, minutes, and seconds
            hours <- floor(elapsed_time / 3600)
            minutes <- floor((elapsed_time %% 3600) / 60)
            seconds <- round(elapsed_time %% 60)
            
            # Format the output string based on the time duration
            if (hours > 0) {
              formatted_time <- sprintf("%dh %dm %ds", hours, minutes, seconds)
            } else {
              formatted_time <- sprintf("%dm %ds", minutes, seconds)
            }
            
            # Print the formatted time
            print(formatted_time)
  
  # Combine results
  df_p <- map_dfr(all_results, ~.x[[1]])
  df_m <- map_dfr(all_results, ~.x[[2]])
  df_l <- map_dfr(all_results, ~.x[[3]])
  
  # Close the parallel plan
  plan(sequential)
  
  # Get the directory of the input file
  output_dir <- dirname(file_path)
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  
  # Create file names for the output
  create_filename <- function(prefix) {
    file.path(output_dir, sprintf("%s_df_%s_%s_%s_%s.csv", timestamp, prefix, from, to, gsub(" ", "_", company_name)))
  }
  
  # Save the dataframes as CSV files
  write_csv(df_p, create_filename("p"))
  write_csv(df_m, create_filename("m"))
  write_csv(df_l, create_filename("l"))
  
  # Return the results as a list (optional, in case you still want to use them in R)
  #return(list(df_p = df_p, df_m = df_m, df_l = df_l))