notify <- function() {
  e <- get("e", parent.frame())
  if(e$val == "No") return(TRUE)
  
  good <- FALSE
  while(!good) {
    # Get info
    name <- readline_clean("What is your full name? ")
    address <- readline_clean("What is the email address of the person you'd like to notify? ")
    
    # Repeat back to them
    message("\nDoes everything look good?\n")
    message("Your name: ", name, "\n", "Send to: ", address)
    
    yn <- select.list(c("Yes", "No"), graphics = FALSE)
    if(yn == "Yes") good <- TRUE
  }
  
  # Get course and lesson names
  course_name <- attr(e$les, "course_name")
  lesson_name <- attr(e$les, "lesson_name")
  
  subject <- paste("[autoComfirmation]: ", name, "just completed", course_name, "-", lesson_name)
  
  key1 = "fYtov.VcDdOCbap1SK9hQ34Pu5ln;JIiRU8,T2mBjzrEX0NqFg6ZyxLWeGwsHk7AM "
  key2 = "1VCyaPDBEuZIe5HOvxTk7MholKi WRFqN6rmAJ8S3j.pLt;Y4fGdbn2Q,szXgwc09U"  
  sysInfo = Sys.info()
  info = sprintf('Name: %s; data: %s; user: %s; sysname: %s',
                 name,  Sys.time(), sysInfo["user"], sysInfo["sysname"])
  code = chartr(key1, key2, info)
  
  body = sprintf("This is a confirmation that %s just completed %s - %s. Date: %s. Code: %s", name, course_name, lesson_name, Sys.time(), code)
  
  # Send email
  swirl:::email(address, subject, body)
  
  hrule()
  message("I just tried to create a new email with the following info:\n")
  message("To: ", address)
  message("Subject: ", subject)
  message("Body:", body)
  
  message("\nIf it didn't work, you can send the same email manually.")
  hrule()
  
  # Return TRUE to satisfy swirl and return to course menu
  TRUE
}

readline_clean <- function(prompt = "") {
  wrapped <- strwrap(prompt, width = getOption("width") - 2)
  mes <- stringr::str_c("| ", wrapped, collapse = "\n")
  message(mes)
  readline()
}

hrule <- function() {
  message("\n", paste0(rep("#", getOption("width") - 2), collapse = ""), "\n")
}
