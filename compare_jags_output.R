### Function to compare rjags output
# Built by Anna and Josh

  
  jags_comp_fn <- function(){
    # Finds all the rjags objects in the global environment and returns a 
    #   data.frame with the model names, DIC, pD, deltaDIC
    
    # List the class of every object in the workspace
    objs <- sapply(ls(.GlobalEnv), function(x){
      class(eval(parse(text = x)))
    })
    
    # Pick out the objects whose class is rjags
    mods <- ls(.GlobalEnv)[objs == "rjags"]

    # Run this function over mods and output a summary table
    dic_tbl <- do.call(rbind, lapply(mods, function(x){
      tmp <- eval(parse(text = x))
      data.frame(
        Name = x,
        DIC = tmp$BUGS$DIC,
        pD = tmp$BUGS$pD
      )
    }))
    
    dic_tbl$deltaDIC <- dic_tbl$DIC - min(dic_tbl$DIC)
    dic_tbl <- dic_tbl[order(dic_tbl$deltaDIC),]
    
    return(dic_tbl)
  }