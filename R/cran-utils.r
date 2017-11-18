detect_python = function() {
  sysinfo = Sys.info()
  if (sysinfo == "Windows") {
    if (file.exists("C:/python27/python.exe"))
      return("C:/python27/python.exe")
    else if (file.exists("C:/python36/python.exe"))
      return("C:/python36/python.exe")
    else
      return(NULL)
  } else {
    if (system('which python', intern = TRUE) != 1L)
      return(basename(system('which python', intern = TRUE)))
    else if (system('which python3', intern = TRUE) != 1L)
      return(basename(system('which python3', intern = TRUE)))
  }
}
