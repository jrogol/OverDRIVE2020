# A function to create a reproducible file structure, along with any custom folders.

createStructure <- function(...) {
  
  exists <- c()
  
  created <- c()
  
  l <- list(...)
  
  if (length(l) == 0) {
    l <- list("Data","Markdown","Output","Reports","SQL","R", "Snippets","Assets")
  }
  
  for (i in l) {
    if (!dir.exists(i)) {
      dir.create(i)
      created <- c(created,i)
    } else {
      exists <- c(exists,i)
    }
  }
  
  if (length(exists) > 0) {
    message(sprintf("Folder(s) already exist for: %s",paste(exists, collapse = ", ")))
  }
  
  if (length(created) > 0) {
    message(sprintf("Created folder(s) for: %s",paste(created, collapse = ", ")))
    
  }
}
