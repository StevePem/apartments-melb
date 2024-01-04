# add car, bike and walk modes based on is_car, is_cycle and is_walk fields

addMode <- function(networkList) {
  nodes <- networkList[[1]]
  links <- networkList[[2]]
  
  links <- links %>%
    mutate(modes=ifelse(                is_car==1,   "car",                          NA)) %>%
    mutate(modes=ifelse(!is.na(modes) & is_cycle==1, paste(modes, "bike", sep=","),  modes)) %>%
    mutate(modes=ifelse( is.na(modes) & is_cycle==1, "bike",                         modes)) %>%
    mutate(modes=ifelse(!is.na(modes) & is_walk==1,  paste(modes, "walk", sep=","),  modes)) %>%
    mutate(modes=ifelse( is.na(modes) & is_walk==1,  "walk",                         modes))

  return(list(nodes,links))
}


