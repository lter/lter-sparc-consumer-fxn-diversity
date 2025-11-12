#' @title Download a Drive Folder to a Local Folder
#' 
#' @description This function downloads all of the CSV files in a given Drive folder into a corresponding local folder on the user's machine. By default, only CSV files in the Drive are identified and downloaded files automatically overwrite local files of the same name (if any exist). There is also an optional argument for specifying a partial string match to use to filter the set of files in Drive to be downloaded
#' 
#' @param folder_url (character) Full hyperlink to the desired Drive folder
#' @param local_subfolder (character) Path--within working directory--to which to download the files found in the Drive folder
#' @param pattern_in (character) _Optional argument_ for using a partial string match to download only matching entries from the Drive folder
#' 
download_drive_folder <- function(folder_url, local_subfolder, pattern_in = NULL) {
  # Error checks for folder
  if(is.null(folder_url) || is.character(folder_url) != T || length(folder_url) != 1 || stringr::str_detect(string = folder_url, pattern = "drive.google.com") != T)
    stop("'folder_url' must be provided and must be a hyperlink to a Drive folder")

  # Error checks for local subfolder
  if(is.null(local_subfolder) || is.character(local_subfolder) != T || length(local_subfolder) != 1)
    stop("'local_subfolder' must be the local file path provided as a character")
  
  # Error checks for pattern (if provided)
  if(is.null(pattern_in) != T){
    if(is.character(pattern_in) != T || length(pattern_in) != 1)
      stop("If provided, 'pattern_in' must be a one-element character vector")
  }
  
  # Convert URL to Drive ID
  drive_id <- googledrive::as_id(folder_url)
  
  # List all files in the folder
  files <- googledrive::drive_ls(path = drive_id, pattern = pattern_in)
  
  # Download each file to the specified local folder
  purrr::walk2(.x = files$id, .y = files$name,
    .f = ~ googledrive::drive_download(file = .x, overwrite = TRUE, type = "csv",
                                       path = file.path(local_subfolder, .y)))
  
  # Return the dribble of downloaded files
  return(files) }

# End ----