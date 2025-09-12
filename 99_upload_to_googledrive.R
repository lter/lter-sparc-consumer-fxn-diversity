#----------------------------------##
# SPARC - Consumer Functional Diversity (CFD) 
###--------------------------------##

# Script author(s): Shalanda Grier, Li Kui, Nick Lyons
# Sites: Palmer LTER, Arctic LTER, North Lakes LTER


# Summary / key points
# 
# After calculations, this loop will upload every .csv from your local 02_community_processed_data and 12_traits_processed_data folders back to the matching Google Drive folders, preserving any nested subfolder structure.
# 
# Files are uploaded as plain CSV (no conversion), overwriting any same-named files.
# assumming there is no nested folders in each of 02_** or 12_** folder

# Load libraries
librarian::shelf(tidyverse, googledrive, fs)

# Clear environment & collect garbage
rm(list = ls()); gc()

# 1) Parent "data" folder on Drive (your link/ID)
parent <- as_id("https://drive.google.com/drive/folders/1AxdFQ0EjNqaLUTzms4prF52cqbVFec0F")

# 2) Local root that holds your processed outputs
local_root <- "data"   # <-- adjust to your local path

# 3) Target subfolders (no nested folders inside these)
targets <- c("02_community_processed_data", "12_traits_processed_data")

# 4) Resolve the two Drive subfolders by name (direct children of parent)
children <- drive_ls(parent, type = "folder")   # lists child folders under parent

for (tname in targets) {
  drive_sub <- children[children$name == tname, ]
  
  # some error checking message
  if (nrow(drive_sub) == 0) {
    warning("Drive subfolder not found under parent: ", tname)
    next
  }
  
  # Local source directory
  local_dir <- path(local_root, tname)
  if (!dir_exists(local_dir)) {
    warning("Local folder not found: ", local_dir)
    next
  }
  
  message("\n== Uploading CSVs from: ", local_dir, " -> Drive:", tname, " ==")
  
  # List CSVs at the top level only (no recursion)
  csvs <- dir_ls(local_dir, recurse = FALSE, type = "file", glob = "*.csv")
  
  if (length(csvs) == 0) {
    message("  (no CSV files found)")
    next
  }
  
  # Upload each CSV "as-is" (no conversion), overwriting if exists
  for (f in csvs) {
    message("⬆️  ", path_file(f))
    
    drive_upload(
      media = f,
      path  = drive_sub,          # upload into the matched Drive folder
      name  = path_file(f),       # keep the same filename
      overwrite = TRUE            # replace if same-named file exists
      # NOTE: do NOT set 'type='; this keeps it as a plain CSV on Drive
    )
  }
}

message("\n✅ Finished uploading CSVs to the two Drive folders.")