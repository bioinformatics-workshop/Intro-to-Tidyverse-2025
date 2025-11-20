library(tibble)
library(dplyr)
library(stringr)

generate_messy_rnaseq_metadata <- function(n_samples = 30) {
  
  set.seed(987)
  
  metadata <- tibble(
    # 1. Primary Identifier
    Sample_ID = paste0("RNA_S", str_pad(1:n_samples, 2, pad = "0")),
    
    # 2. Encoded Information (Needs separating/parsing)
    `Patient-Treatment-Batch` = paste0(
      sample(paste0("P", 101:108), n_samples, replace = TRUE), "_",
      sample(c("DrugA", "Placebo"), n_samples, replace = TRUE), "_",
      sample(c("B1", "B2", "B3"), n_samples, replace = TRUE)
    ),
    
    # 3. Inconsistent Categorical Factor (Needs standardization/coalescing)
    `TREATMENT Group` = sample(
      c("High_Dose", "Low-Dose", "Control", "NA"), 
      n_samples, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1)
    ),
    
    # 4. Sequencing Depth (Numeric with NAs)
    Sequencing_Depth_M = rnorm(n_samples, mean = 75, sd = 25) %>% round(1),
    
    # 5. Sex/Gender (Inconsistent spelling and NAs)
    `Sex/Gender` = sample(
      c("Male", "Female", "NA"), 
      n_samples, replace = TRUE, prob = c(0.4, 0.5,0.1)
    ),
    
    # 6. Collection Temperature (Character with unit, needs type conversion)
    Collection_Temp = paste0(sample(c(25, 26, 27), n_samples, replace = TRUE), " C"),
    
    # 7. QC Score (Numeric, but with an outlier/error)
    `QC Score (min)` = rpois(n_samples, lambda = 15) * sample(c(1, 0.1), n_samples, replace = TRUE, prob = c(0.9, 0.1)),
    
    # --- NEW COLUMNS ADDED BELOW ---
    
    # 8. RNA Concentration (Numeric, with NA/missing data)
    `RNA Concentration ng/µL` = rnorm(n_samples, mean = 150, sd = 30) %>% round(2),
    
    # 9. Technician/Rater (Inconsistent casing, needs standardization)
    `Technician Name` = sample(c("Alice", "alice", "Bob", "BOB", "Charlie"), n_samples, replace = TRUE),
    
    # 10. Timepoint (Mixed type: numeric and character string)
    Timepoint_Days = sample(c(7, 14, 21, "NA"), n_samples, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1)),
    
    # 11. Sequencing Platform (Categorical, needs standardization)
    Sequencer_Platform = sample(c("Illumina", "NovaSeq", "MGI", "PacBio"), n_samples, replace = TRUE, prob = c(0.4, 0.1, 0.4, 0.1))
  )
  
  # Inject explicit NAs and errors
  metadata$`Sequencing_Depth_M`[sample(1:n_samples, size = 3)] <- NA
  metadata$`Sex/Gender`[sample(1:n_samples, size = 2)] <- "undetermined"
  metadata$`RNA Concentration ng/µL`[sample(1:n_samples, size = 4)] <- NA 
  
  # Rename a column to have an awkward character for cleaning practice
  metadata <- metadata %>% rename(`Sequencing Depth (M)` = Sequencing_Depth_M)
  
  return(metadata)
}

# Generate the messy sample sheet
rnaseq_metadata <- generate_messy_rnaseq_metadata(n_samples = 100)

message("--- Messy RNA-seq Sample Metadata (Extended) ---")
print(head(rnaseq_metadata))

write_csv(rnaseq_metadata, "data/metadata.csv", col_names = TRUE)
