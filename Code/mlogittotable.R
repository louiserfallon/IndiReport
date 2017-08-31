mlogittotable <- function(model, keys=rownames(summary(model)$CoefTable), formula=(model$call)$formula)
{ #make a summary of the model, not displayed, required to access the model variables later
  #(interesting quirk of logistic pglm model maxlik output objects)
  summary(model)
  
  #generate row names based on keys provided
  dfrownames <- c("McFadden R2",keys)
  #create column containing variables and their statistical significance
  
  tablecol <- c(format(round(summary(model)$mfR2[1],5)))
  for (key in keys)
    #check if the key is in the model, if not then add 2 blank lines
  {if (!key %in% rownames(summary(model)$CoefTable)) {tablecol <- c(tablecol,"") } else
    #otherwise, add the variable, multiplied by the multiplier,
    #and add the appropriate *s or .s based on the p value
  {if (summary(model)$CoefTable[key,4] <= 0.001) signif <- "(***)" else
    if (summary(model)$CoefTable[key,4] <= 0.01) signif <- "(**)" else
      if (summary(model)$CoefTable[key,4] <= 0.05) signif <- "(*)" else
        if (summary(model)$CoefTable[key,4] <= 0.1) signif <- "(.)" else
          if (summary(model)$CoefTable[key,4] <= 1) signif <- ""
          tablecol <- c(tablecol,paste(format(round(summary(model)$CoefTable[key,1],4),digits=5),signif))}
  }
  return(data.frame(variables=dfrownames,
                    values=tablecol))
}