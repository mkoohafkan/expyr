.onLoad = function(libname, pkgname){ 
  pythonenv = new.env(parent = getNamespace("pysockr"))
  assign("running", FALSE, pythonenv)
  assign("pythonenv", pythonenv, envir = getNamespace("pysockr"))
}

.onAttach = function(libname, pkgname){
}

.onDetach = function(libname){
}