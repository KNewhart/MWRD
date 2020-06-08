printToPowerPoint <- function(code.block, presentation.path = NULL, type = "figure", title = NULL) {
  library(officer)
  library(magrittr)
  library(rvg)
  library(xts)
  # library(flextable)
  
  # my_pres <- read_pptx()
  # my_pres <- read_pptx(path = "my_pres.pptx")
  # layout_summary(my_pres)
  
  it <- getwd()
  it <- paste0(it,"/my_pres.pptx")
  if(!is.null(presentation.path)) presentation.path <- paste0(presentation.path,"/my_pres.pptx")
  if(is.null(presentation.path)) presentation.path <- it
  if(!file.exists(presentation.path)) my_pres <- read_pptx(path = "C:/Users/kbnewhart/Desktop/my_pres.pptx")
  if(file.exists(presentation.path)) my_pres <- read_pptx(path = presentation.path)
  
  if(type=="figure") {
    my_pres <- my_pres %>% 
      add_slide(layout="Only Content")
    my_pres <- ph_with_vg(my_pres, code=code.block, type="body")
  }
  
  if(type=="table") {
    if(is.null(title)) {
      my_pres <- my_pres %>% 
        add_slide(layout="Only Content")
    } else {
      my_pres <- my_pres %>% 
        add_slide(layout="Title and Content") %>%
        ph_with(value=title, location=ph_location_type(type="title"))
    }
    my_pres <- ph_with_table(x=my_pres, value = code.block, first_column = TRUE)
  }
  
  if(type=="text") {
    if(is.null(title)) {
      my_pres <- my_pres %>% 
        add_slide(layout="Only Content")
    } else {
      my_pres <- my_pres %>% 
        add_slide(layout="Title and Content") %>%
        ph_with(value=title, location=ph_location_type(type="title"))
    }
    my_pres <- ph_with(x=my_pres, value=code.block, location=ph_location_type(type="body"))
  }
  
  print(my_pres, presentation.path)
}