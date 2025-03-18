#Author: Alex Zajichek
#Date: 10-17-2017
#Description: Will call Java functions to write RandomForest object to a
#RandomAccessFile (Java class) where it can be processed by JRFEvaluate

#1) Call first constructor to write out RF meta data and initialize file
#2) Call second constructor to write out each tree to the same file
JForest2txt <- function(object) {
  
  #Type of forest (T = regression, F = classification)
  forest_type <- object$type
  forest_type <- ifelse(forest_type == 'regression', T, F)
    
  #Data types of all variables (T = factor, F = numeric)
  forest_vars_types <- unlist(lapply(sapply(mod_dat, class), function(x){x[1]}))
  forest_vars_types <- ifelse(forest_vars_types %in% c('ordered','factor'), T, F)
  
  
  #JCALL 'initializeRegressionForest' (forest_vars_types, "myforest.forest")
  #or initializeClassificationForest: pass number of classes
  #This will return an integer which is the location to start writing the trees
  
  
  #.jcall(obj,"V","try2dDoubleArray",
   #      .jarray(doubleMatrix, dispatch = T))
  #V = return type is void
  #doubleMatrix is the matrix of doubles
  
  #USE THIS ALGORITHM TO EDIT CLASSIFICATION TREES BEFORE SENDING TO JAVA
  term_nodes <- as.numeric(which(tree[,6] != 0))
  left_term_nodes <- term_nodes[term_nodes %% 2 == 0]
  right_term_nodes <- term_nodes[term_nodes %% 2 != 0]
  tree[which(tree[,1] %in% left_term_nodes), 1] <- tree[left_term_nodes, 6]*-1
  tree[which(tree[,2] %in% right_term_nodes), 2] <- tree[right_term_nodes, 6]*-1
  tree <- tree[tree[,5] != -1, -c(5,6)]
  #Remapping
  row_names <- as.numeric(rownames(tree))
  tree[,1][tree[,1] > 0] <- which(row_names %in% tree[,1])
  tree[,2][tree[,2] > 0] <- which(row_names %in% tree[,2])
  
  

}