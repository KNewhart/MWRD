save.results <- function(obj, obj.folder, obj.name=NULL, results.folder = paste0(getwd(),"/results")) {
  dir.create(file.path(results.folder, obj.folder), showWarnings = FALSE)
  if(is.null(obj.name)) as.character(bquote(obj))
  save(obj, file=file.path(results.folder, obj.folder,paste0(obj.name,".RData")))
}