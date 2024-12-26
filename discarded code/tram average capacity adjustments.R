# tram average capacity adjustments
# these were used where Yarra Trams specified a mixed class (eg a/z), but 
# ultimately it was possible to make assumptions about all such instances

class.cap <- class.cap %>%
  # add alternative class names
  rbind(.,
        c("z3", as.numeric(class.cap[class.cap$class == "z", "capacity"])),
        c("e1", as.numeric(class.cap[class.cap$class == "e", "capacity"]))) %>%
  # add unclear classes [where Yarra Trams data doesn't specify a single class] to class.cap
  rbind(., 
        c("a/z", (as.numeric(class.cap[class.cap$class == "a", "capacity"]) +
                    as.numeric(class.cap[class.cap$class == "z", "capacity"])) / 2),
        c("e/c2", (as.numeric(class.cap[class.cap$class == "e", "capacity"]) +
                     as.numeric(class.cap[class.cap$class == "c2", "capacity"])) / 2))

