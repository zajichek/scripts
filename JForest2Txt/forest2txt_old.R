#Write out forest to file
#0) Print variable names, types in order; print factor levels
#1) 1 line = 1 tree
#2) Separate nodes by ;
#3) Separate fields within a node by :
#4) Order - <varName>:<value> (or categories):<leftnode>:<rightnode>
#5) Write value of terminal node in the position where it point to it to save room
#If formula was not used, must supply var_types manually as a named vector

forest2txt <- function(object, destination, mod_dat, write_forest = TRUE, return_lines = FALSE, num_trees = object$ntree) {
  
  #Data types of all variables
  forest_vars_types <- unlist(lapply(sapply(mod_dat, class), function(x){x[1]}))
  
  #Names of all variables
  forest_vars <- names(forest_vars_types)
  
  #Storing all lines here
  contains_factor <- ('factor' %in% forest_vars_types) | ('ordered' %in% forest_vars_types)
  forest_lines <- array(NA, 3 + contains_factor + num_trees)
  
  #Storing first two lines
  forest_lines[1] <- object$type
  if(forest_lines[1] == 'classification') {
    forest_lines[1] <- paste0(forest_lines[1], ":", paste(1:length(object$classes), collapse = ":"))
  }
  forest_lines[2] <- paste(forest_vars, collapse = ":")
  forest_lines[3] <- paste(forest_vars_types, collapse = ":")
  start <- 4
  
  #If at least one factor exists, write new line with all levels
  if(contains_factor) {
    #All factors in forest
    all_factors <- forest_vars[forest_vars_types %in% c('factor', 'ordered')]
    
    #Subset of data containing factors
    temp_dat <- mod_dat[, all_factors, drop = FALSE]
    
    #Pulling levels of each factor
    factor_levels <- ""
    for(i in 1:ncol(temp_dat)) {
      levs <- levels(temp_dat[,i])
      factor_levels <- paste0(factor_levels, paste(levs,collapse = ":"), ";")
    }
    forest_lines[4] <- factor_levels
    start <- 5
  }
  
  #Looping through each tree
  for(i in start:length(forest_lines)) {
    tree <- getTree(object, k = i-(start - 1))
    tree_line <- ""
    for(j in 1:nrow(tree)) {
      var_name <- as.character(tree[j,3])
      if(var_name != 0) {
        left <- tree[j,1]
        right <- tree[j,2]
        val <- tree[j,4]
        tree_line <- paste0(tree_line, paste(var_name, val, left, right, sep = ":"),";")
      } else {
        tree_line <- paste0(tree_line, "0:",tree[j,6],";")
      }
    }
    forest_lines[i] <- tree_line
    print(i)
  }
  if(write_forest) {
    write.table(forest_lines, file = destination, 
                row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  if(return_lines) {
    return(forest_lines)
  }
}