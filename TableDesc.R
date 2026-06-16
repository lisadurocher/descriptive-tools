
######################################################################################


'%ni%' <- Negate('%in%')


#### getresult : to get mean +- SD and n(%) ####
#Function to get moy+ET (or med(Q1-Q3) if not normal) for numeric and n(%) for factors
##EX: #CTRL+SHIFT+C to comment/uncomment below
# data("mtcars")
# mtcars$am=as.factor(mtcars$am)
# mtcars$mpg=as.numchar(mtcars$mpg)
# getresult(data=mtcars,var="am") #For factors level needs to be specified
# getresult(data=mtcars,var="am",levelj="1")
# getresult(data=mtcars,var="mpg",virg=2) #By default 0 after virg
# #Normality: 1 = normal, 0 = not normal
# getresult(data=mtcars,var="mpg",normal=0) #If normality is not specified it is calculated using checknormality function
getresult <- function(data,var,levelj=NULL,virg=NULL,virg.percent=NULL,normal=NULL){
  
  x=NULL
  y=NULL
  
  #If normal is empty then use checknormality function
  if (is.null(normal)) {
    normal=checknormality(data=data,var=var)
  }
  
  
  #FACTOR
  if (is.factor(data[,var])) {
    #Only NA then return "-":
    if (all(is.na(data[,var]))) {return("-")}
    
    tot=sum(table(data[,var]))
    x=table(data[,var])[which(levels(data[,var])==levelj)]
    y=format(round(x/tot *100,virg.percent), nsmall=virg.percent)
    return(paste(x," (",y,"%)",sep=""))
  }
  
  #NUMERIC
  if (is.numeric(data[,var])) {
    #If not normal then median + Q1 and Q3
    if (normal==0) { 
      #Only NA then return nothing:
      if (all(is.na(data[,var]))) {return("-")}
      
      x=format(round(median(data[,var],na.rm=TRUE),virg), nsmall = virg)
      y=format(round(quantile(data[,var],na.rm=TRUE,type=2)[2],virg), nsmall = virg)
      z=format(round(quantile(data[,var],na.rm=TRUE,type=2)[4],virg), nsmall = virg)
      return(paste(x," (",y,"-",z,")",sep=""))
      
    } else {
      #Only NA then return nothing:
      if (all(is.na(data[,var]))) {return("-")}
      
      x=format(round(mean(data[,var],na.rm=TRUE),virg), nsmall = virg)
      y=format(round(sd(data[,var],na.rm=TRUE),virg), nsmall = virg)
      return(paste(x,stringr::str_conv("\xb1","ISO-8859-1"),y))
      #Only unicode string http://unicode.scarfboy.com/?s=U%2b00b1
    }
  }
}




##########################


#### Check normality: returns 0 or 1 (1 implies normality) ####
#Uses the Shapiro test to evaluate normality.
# If no TRT is specified it only checks normality in the total population but if
#   a TRT is specified it will check normality in the global pop and for each TRT pop.
# Normality is returned if in the global pop and all TRT pops the variable is normally distributed.
##EX: #CTRL+SHIFT+C to comment/uncomment below
# data("mtcars")
# checknormality(data=mtcars,var="qsec")
# qqnorm(mtcars$qsec,main="QQ plot of data",pch=19)
# qqline(mtcars$qsec, col = 2)
# checknormality(data=mtcars,var="disp")
# qqnorm(mtcars$disp,main="QQ plot of data",pch=19)
# qqline(mtcars$disp, col = 2)
# checknormality(data=mtcars,var="am") #All factors will automatically return 0
checknormality <- function(data,var,TRT=NULL) {
  data=droplevels(data)
  if (is.null(TRT)) {
    if (is.numeric(data[,var])) {
      #Cannot use Shapiro with less than 3 values, returns not normal:
      if (length(table(data[,var])) < 3) { return(0)}
      if (class(try(shapiro.test(data[,var]),silent=TRUE))=="try-error") {stop(paste0("Shapiro test did not work, check if ",var," is supposed to be numeric or check the data."))}
      if (shapiro.test(data[,var])$p < 0.05) {return(0)} else {return(1)}
    } else {return(0)} #if not numeric thus factor
  } else {
    if (!is.numeric(data[,var])) {return(0)} #if not numeric then quali thus not normal
    #Normality in each TRT group:
    data[,TRT]=as.factor(data[,TRT])
    for (i in 1:length(levels(data[,TRT]))) {
      if (sum(table(data[,var],data[,TRT])[,i])<3) {return(0)} #sum and not length because it is per group and thus has 0 for some values
      if (class(try(shapiro.test(data[,var][which(data[,TRT]==levels(data[,TRT])[i])]),silent=TRUE))=="try-error") {stop(paste0("Shapiro test did not work, check if ",var," is supposed to be numeric or check the data."))}
      if (shapiro.test(data[,var][which(data[,TRT]==levels(data[,TRT])[i])])$p < 0.05) {return(0)}
    }
    #Normality for the global pop
    if (class(try(shapiro.test(data[,var]),silent=TRUE))=="try-error") {stop(paste0("Shapiro test did not work, check if ",var," is supposed to be numeric or check the data."))}
    if (shapiro.test(data[,var])$p < 0.05) {return(0)}
    
    #If all TRT groups and the global pop. are all normal then no return was triggered and the var is normal
    #normal in each TRT group and the global pop thus 1 is returned
    return(1)
  }
}


#############



#### FTdesc :  to get table with getresults and labels ####

#Can be used on its own to get means and freq for the global pop but no FlexTable is returned
#FlexTable = Table that can be exported to Word
##EX: #CTRL+SHIFT+C to comment/uncomment below
# data("mtcars")
# mtcars$am=as.factor(mtcars$am)
# mtcars$cyl=as.factor(mtcars$cyl)
# mtcars$vs=as.factor(mtcars$vs)
# vars=c("mpg","cyl","hp","vs")
# labels=c("Miles/(US) gallon" , "Number of cylinders" ,"Gross horsepower" , "V/S")
# nolevels=vars
# mtcars$vs=relevel(mtcars$vs,ref="1")
# #nolevels means that any variable with only 2 levels will be on a single line and the first level will be used, thus relevel needed for some
# FTdesc(data=mtcars,vars=vars,labels=labels,nolevels=vars,virg=c(1,2,3))
# #virg can be specified: it's the amount of decimals used for the numeric variable
# #The \t means a tabulation in a word document, used for all levels of a factor
# #reference is TRUE by default, any other value removes the ", n (%)" after the label for categorical variables
FTdesc <- function(data,vars,labels,virg=NULL,virg.percent=0,nolevels=NULL,normal=NULL,reference=TRUE, MISSING=TRUE){
  data=droplevels(data)
  #Setting virg to the correct length:
  if (is.null(virg)) {
    virg=0
  } else {
    #If virg length = vars length then AT LEAST all numeric variables have a value of virg specified
    if (length(virg) >1) {
      virg=virg[1]
      warning("virg should be a unique value")
    }
  }
  
  
  #If normal is empty then use checknormality function
  if (is.null(normal)) {
    normal=c()
    for (i in 1:length(vars)) {
      normal=c(normal,checknormality(data=data,var=vars[i]))
    }
  }
  
  tab=data.frame(A= character(), B= character())
  tab2=data.frame(A= character(), B= character())
  if (length(vars) != length(labels)) {stop("Varlist and labels are not the same length.")}
  for (i in 1:length(vars)) {
    if (is.factor(data[,vars[i]])) {
      # If in nolevels and length = 2 then 1 line: (OR 1 level meaning 100% answered by this 1 level; EX: only YES answers)
      if (vars[i] %in% nolevels && length(levels(data[,vars[i]]))==2 ) { 
        if (reference == TRUE) {tab2=data.frame(A=c(paste0(labels[i],", n (%)")),B=getresult(data=data,var=vars[i],levelj=levels(data[,vars[i]])[1],virg=virg,virg.percent=virg.percent))} 
        else {tab2=data.frame(A=c(paste0(labels[i],"")),B=getresult(data=data,var=vars[i],levelj=levels(data[,vars[i]])[1],virg=virg,virg.percent=virg.percent))}
        tab=rbind(tab,tab2)
      } else {
        #Add exception, if length levels = 0 then run code above and return "-"
        if (all(is.na(data[,vars[i]]))) {
          if (reference == TRUE ) {tab2=data.frame(A=c(paste0(labels[i],", n (%)")),B=getresult(data=data,var=vars[i],levelj=levels(data[,vars[i]])[1],virg=virg,virg.percent=virg.percent))} 
          else {tab2=data.frame(A=c(paste0(labels[i],"")),B=getresult(data=data,var=vars[i],levelj=levels(data[,vars[i]])[1],virg=virg,virg.percent=virg.percent))}
          tab=rbind(tab,tab2)
        } else {
          #Else multiple rows, one for each level
          if (reference == TRUE) {tab2=data.frame(A=c(paste0(labels[i],", n (%)")),B="")} else {tab2=data.frame(A=c(paste0(labels[i],"")),B="")}
          tab=rbind(tab,tab2)
          for (j in levels(data[,vars[i]])) {
            tab2=data.frame(A=c(paste0("\t",j)),B=getresult(data=data,var=vars[i],levelj=j,virg=virg,virg.percent=virg.percent)) 
            tab=rbind(tab,tab2)
          }
        }
        
      }
    } else {
      tab2=data.frame(A=c(paste0(labels[i])),B=getresult(data=data,var=vars[i],virg=virg,virg.percent=virg.percent,normal=normal[i]))
      tab=rbind(tab,tab2)
    }
    
    #if at least one missing value, add a row
    if(MISSING == TRUE){
      if(any(is.na(data[,vars[i]]))){
        tab=rbind(tab, 
                  data.frame(A="Missing (n)", B=sum(is.na(data[,vars[i]])*1)))
      }
    }
  }
  return(tab)
}








#### comptest : to get the pvalue with the corresponding test ####

##EX: #CTRL+SHIFT+C to comment/uncomment below
# data("mtcars")
# mtcars$am=as.factor(mtcars$am)
# table(mtcars$am) #TRT group
# mtcars$cyl=as.factor(mtcars$cyl)
# comptest(data=mtcars,var="cyl",grp="am")
# table(mtcars$cyl,mtcars$am) #effectifs < 5 donc fisher
# chisq.test(mtcars$cyl,mtcars$am,correct=FALSE)
# fisher.test(mtcars$cyl,mtcars$am)
# comptest(data=mtcars,var="hp",grp="am",nrpval=5)
# qqnorm(mtcars$hp) #checknormality returns 0, not normal, it is possible to force it with normal option
# comptest(data=mtcars,var="hp",grp="am",normal=1,nrpval=3)
comptest <- function(data,var,grp,normal=NULL,nrpval,paired) {
  data=droplevels(data)
  #if no nrpval is specified then 4 is used by default
  if (missing(nrpval)) {nrpval=4}
  
  #Sets the lower than value using nrpval
  lowervalch=paste0("0.",paste(rep(0,nrpval-1),collapse=""),"1")
  lowerval=as.numeric(as.character(lowervalch))
  lowervalch=paste0("<",lowervalch)
  
  
  #If normal is empty then use checknormality function
  if (is.null(normal)) {
    normal=checknormality(data=data,var=var,TRT=grp)
  }
  
  
  if(paired == FALSE){
    if (is.factor(data[,var])) {
      
      #If only values in 1 group then return without test:
      if (sum(table(data[,grp],data[,var])[1,])==0 || sum(table(data[,grp],data[,var])[2,])==0) {
        pvalue <- "-"
        test <- "-"
      }else if(length(levels(droplevels(data[,var])))==1){
        pvalue <- "-"
        test <- "-"
      }else{
        if (any(suppressWarnings(chisq.test(data[,var],data[,grp],correct=FALSE))$expected<5)) {
        #If more than 10 categories send a warning that the fisher test might take very long to run
        if (dim(suppressWarnings(chisq.test(data[,var],data[,grp],correct=FALSE)$resid))[1] > 20) {warning(paste0("Fisher test started with >20 categories, this test might take very long",". For variable: ",var))}
        #If an error with the fisher test then the pvalue will be simulated by repeating the simulation 100k times
        if (class(try(fisher.test(data[,var],data[,grp]),silent = TRUE))=="try-error") {
          if (fisher.test(data[,var],data[,grp],simulate.p.value=TRUE,B=1e5)$p.value < lowerval) {
            pvalue <- lowervalch
          } else {
            pvalue <- formatC(fisher.test(data[,var],data[,grp],simulate.p.value=TRUE,B=1e5)$p.value, format='f', digits=nrpval)
          }
        }
        if (fisher.test(data[,var],data[,grp])$p.value < lowerval) {
          pvalue <- lowervalch
        } else {
          pvalue <- formatC(fisher.test(data[,var],data[,grp])$p.value, format='f', digits=nrpval )
          
        }
        test <- "Fisher exact test"
      } else {
        if (chisq.test(data[,var],data[,grp],correct=FALSE)$p.value < lowerval) {pvalue <- lowervalch} else {pvalue <- formatC(chisq.test(data[,var],data[,grp],correct=FALSE)$p.value, format='f', digits=nrpval )}
        test <- "Chi-squared test"
      }
      }
      
      
      #Fisher is used if any of the expected values is less than 5
      #(To check for expected you must first do a chisq.test then X$expected, thus warnings)
      
      
    } else {
      #If only values in 1 group then return without test:
      if (sum(table(data[,grp],data[,var])[1,])==0 || sum(table(data[,grp],data[,var])[2,])==0) {
        pvalue <- "-"
        test <- "-"
      }else{
        #normal==0 means that it is not normal
      if (normal==0) {
        
        #If only 2 groups wilcoxon
        if (length(names(table(data[,grp]))) < 3) {
          #If wilcox pvalue is NA then return -
          #The exact=FALSE is for when there are ties (if none then pvalue is a little different but barely)
          if (is.na(suppressWarnings(wilcox.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var]))$p.value)) {
            pvalue <- "-"
            test <- "-"} 
          if (suppressWarnings(wilcox.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var]))$p.value < lowerval) {
            pvalue <- lowervalch
            test <- "Wilcoxon–Mann–Whitney test"
          } else {
            pvalue <- formatC( suppressWarnings(wilcox.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var]))$p.value, format='f', digits=nrpval )
            test <- "Wilcoxon–Mann–Whitney test"
          }
          
          
          
          #Else Kruskall Wallis
        } else {
          if (kruskal.test(data[,var] ~ data[,grp])$p.value < lowerval) {
            pvalue <- lowervalch
          } else { pvalue <- formatC(kruskal.test(data[,var] ~ data[,grp])$p.value, format='f', digits=nrpval )}
          test <- "Kruskal-Wallis test"
        }
      } else {
        
        #If only 2 groups student
        if (length(names(table(data[,grp]))) < 3) {
          #Bartlett test to check for equal variances for the following t.test
          #Possible with leveneTest() from car library (Apparently: Bartlett better for small pop sizes but Levene better in general)
          if (bartlett.test(data[,var],data[,grp])$p.value < 0.05) {
            if (t.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var])$p.value < lowerval) {
              pvalue <- lowervalch
            } else {pvalue <- formatC( t.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var])$p.value, format='f', digits=nrpval )}
            test <- "Student's t test"
          } else {
            if (t.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var],var.equal=TRUE)$p.value < lowerval) {
              pvalue <- lowervalch
            } else {pvalue <- formatC( t.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var],var.equal=TRUE)$p.value, format='f', digits=nrpval )}
            test <- "Student's t test"
          }
          #Else ANOVA
        } else {
          test <- "ANOVA"
          #if not equal variances use a kruskal.test instead
          if (bartlett.test(data[,var],data[,grp])$p.value < 0.05) {
            if (kruskal.test(data[,var] ~ data[,grp])$p.value < lowerval) {
              pvalue <- lowervalch
            } else {pvalue <- formatC(kruskal.test(data[,var] ~ data[,grp])$p.value, format='f', digits=nrpval )}
            test <- "Kruskal-Wallis test"}
          if (summary(aov(data[,var] ~ data[,grp]))[[1]][["Pr(>F)"]][1] < lowerval) {
            pvalue <- lowervalch
          } else {pvalue <- formatC(summary(aov(data[,var] ~ data[,grp]))[[1]][["Pr(>F)"]][1], format='f', digits=nrpval )}
        }
      }
      }
      
      
    }}else{
      if(sum(table(data[,grp],data[,var])[1,]) != sum(table(data[,grp],data[,var])[2,])){
        warning(paste0("Paired group must have the same number of observations. For variable:", var))
        pvalue <- "-"
        test <- "-"
      }
      if(sum(is.na(data[,var])) > 0) {
        warning(paste0("Variable ", var, " have missing value. Check if your data is correctly matched."))
      }
      
      if (is.factor(data[,var])) {
        
        #If only values in 1 group then return without test:
        if (sum(table(data[,grp],data[,var])[1,])==0 || sum(table(data[,grp],data[,var])[2,])==0) {
          pvalue <- "-"
          test <- "-"
        }else if(length(levels(droplevels(data[,var])))==1){
          #If only 1 level then return without test:
          pvalue <- "-"
          test <- "-"
        }else{
          #Fisher is used if any of the expected values is less than 5
        #(To check for expected you must first do a chisq.test then X$expected, thus warnings)
        if (any(suppressWarnings(chisq.test(data[,var],data[,grp],correct=FALSE))$expected<5)) {
          #If more than 10 categories send a warning that the fisher test might take very long to run
          if (dim(suppressWarnings(chisq.test(data[,var],data[,grp],correct=FALSE)$resid))[1] > 20) {warning(paste0("Fisher test started with >20 categories, this test might take very long",". For variable: ",var))}
          #If an error with the fisher test then the pvalue will be simulated by repeating the simulation 100k times
          if (class(try(fisher.test(data[,var],data[,grp]),silent = TRUE))=="try-error") {
            if (fisher.test(data[,var],data[,grp],simulate.p.value=TRUE,B=1e5)$p.value < lowerval) {pvalue <- lowervalch} else {pvalue <- formatC(fisher.test(data[,var],data[,grp],simulate.p.value=TRUE,B=1e5)$p.value, format='f', digits=nrpval )}
          }
          if (fisher.test(data[,var],data[,grp])$p.value < lowerval) {pvalue <- lowervalch} else {pvalue <- formatC(fisher.test(data[,var],data[,grp])$p.value, format='f', digits=nrpval )}
          test <- "Fisher exact test"
        } else {
          if (mcnemar.test(data[,var],data[,grp],correct=FALSE)$p.value < lowerval) {pvalue <- lowervalch} else {pvalue <- formatC(mcnemar.test(data[,var],data[,grp],correct=FALSE)$p.value, format='f', digits=nrpval )}
          test <- "McNemar's test"
        }
        }
        
      } else {
        #If only values in 1 group then return without test:
        if (sum(table(data[,grp],data[,var])[1,])==0 || sum(table(data[,grp],data[,var])[2,])==0) {
          pvalue <- "-"
          test <- "-"
        }else{
          #normal==0 means that it is not normal
        if (normal==0) {
          
          #If only 2 groups wilcoxon
          if (length(names(table(data[,grp]))) < 3) {
            #If wilcox pvalue is NA then return -
            #The exact=FALSE is for when there are ties (if none then pvalue is a little different but barely)
            if (is.na(suppressWarnings(wilcox.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var],paired = TRUE))$p.value)) {
              pvalue <- "-"
              test <- "-"} 
            if (suppressWarnings(wilcox.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var],paired=TRUE))$p.value < lowerval) {
              pvalue <- lowervalch
              test <- "Wilcoxon signed-rank test"
            } else {pvalue <- formatC( suppressWarnings(wilcox.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var],paired=TRUE))$p.value, format='f', digits=nrpval )
            test <- "Wilcoxon signed-rank test"}
            
          } else {
            pvalue <- "-"
            test <- "-"
          }
        } else {
          
          #If only 2 groups student
          if (length(names(table(data[,grp]))) < 3) {
            #Bartlett test to check for equal variances for the following t.test
            #Possible with leveneTest() from car library (Apparently: Bartlett better for small pop sizes but Levene better in general)
            if (bartlett.test(data[,var],data[,grp])$p.value < 0.05) {
              if (t.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var],paired=TRUE)$p.value < lowerval) {
                pvalue <- lowervalch
              } else {
                pvalue <- formatC( t.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var],paired=TRUE)$p.value, format='f', digits=nrpval )
              }
              test <- "t-test for matched pairs"
            } else {
              if (t.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var],var.equal=TRUE, paired=TRUE)$p.value < lowerval) {
                pvalue <- lowervalch
              } else {
                pvalue <- formatC( t.test(data[which(data[,grp]==names(table(data[,grp]))[1]),][,var],data[which(data[,grp]==names(table(data[,grp]))[2]),][,var],var.equal=TRUE,paired=TRUE)$p.value, format='f', digits=nrpval )
              }
              test <- "t-test for matched pairs"
            }
            #Else ANOVA
          } else {
            pvalue <- "-"
            test <- "-"
          }
        }
          
        }
      }
    }
  return(c(pvalue, test))
}




#### smd : to get the standardized Mean Difference ####


SMD <- function(data,var,TRT,label,nolevels=NULL, MISSING = TRUE){
  nbvar <- length(var)
  smd <- c()
  varnames <- c()
  for (i in 1:nbvar) {
    if (is.numeric(data[,var[i]])){
      z1 <- mean(data[,var[i]][data[,TRT] == levels(data[,TRT])[2]], na.rm=TRUE)
      z0 <- mean(data[,var[i]][data[,TRT] == levels(data[,TRT])[1]], na.rm=TRUE)
      s1 <- var(data[,var[i]][data[,TRT] == levels(data[,TRT])[2]], na.rm=TRUE)
      s0 <- var(data[,var[i]][data[,TRT] == levels(data[,TRT])[1]], na.rm=TRUE)
      smd <- c(smd,format(round((abs(z1-z0)/ sqrt((s1+s0)/2)),3),nsmall=3))
      varnames <- c(varnames, label[i])
    }
    if(is.factor(data[,var[i]])){
      if (length(levels(data[,var[i]])) == 2) {
        tot1 <- length(data[,var[i]][!is.na(data[,var[i]]) & data[,TRT] == levels(data[,TRT])[1]])
        tot0 <- length(data[,var[i]][!is.na(data[,var[i]]) & data[,TRT] == levels(data[,TRT])[2]])
        p1 <- length(data[,var[i]][data[,var[i]]==levels(data[,var[i]])[2] &
                                     !is.na(data[,var[i]]) & data[,TRT] == levels(data[,TRT])[1]])/tot1
        p0 <- length(data[,var[i]][data[,var[i]]==levels(data[,var[i]])[2] &
                                     !is.na(data[,var[i]]) & data[,TRT] == levels(data[,TRT])[2]])/tot0
        smd <- c(smd,format(round((abs(p1-p0)/sqrt((p1*(1-p1)+p0*(1-p0))/2)),3),nsmall=3))
        varnames <- c(varnames, label[i])
        if(var[i] %ni% nolevels){
          varnames <- c(varnames, levels(data[,var[i]]))
          smd <- c(smd, rep("", length(levels(data[,var[i]])))) 
        }
      }else if (length(levels(data[,var[i]])) == 1) {
        smd <- c(smd, "-")
        varnames <- c(varnames, label[i])
        if(var[i] %ni% nolevels){
          smd <- c(smd, "")
          varnames <- c(varnames, levels(data[,var[i]]))
        }
      }else if (length(levels(data[,var[i]])) > 2) {
        
        tot1 <- length(data[,var[i]][!is.na(data[,var[i]]) & data[,TRT] == levels(data[,TRT])[1]])
        tot0 <- length(data[,var[i]][!is.na(data[,var[i]]) & data[,TRT] == levels(data[,TRT])[2]])
        mat <- matrix(data = NA,nrow = length(levels(data[,var[i]])), ncol = 2, byrow = F)
        
        for (j in 1:length(levels(data[,var[i]]))) {
          mat[j,1] <- length(data[,var[i]][data[,var[i]]==levels(data[,var[i]])[j] &
                                             !is.na(data[,var[i]]) & data[,TRT] == levels(data[,TRT])[1]]) / tot1
          mat[j,2] <- length(data[,var[i]][data[,var[i]]==levels(data[,var[i]])[j] &
                                             !is.na(data[,var[i]]) & data[,TRT] == levels(data[,TRT])[2]]) / tot0
        }
        
        Tt <- mat[-1,1]
        C <- mat[-1,2]
        
        meanDiffVector<-(Tt-C)
        vcovT <- -1 * outer(Tt, Tt)
        diag(vcovT) <- Tt * (1-Tt)
        
        vcovC <- -1 * outer(C, C)
        diag(vcovC) <- C * (1-C)
        
        S<-(vcovC+vcovT)/2
        
        Sinv <- solve(S)
        
        smdCat <- drop(t(meanDiffVector) %*% Sinv %*% t(t(meanDiffVector)))
        smdCat <- sqrt(drop(t(meanDiffVector) %*% Sinv %*% meanDiffVector))
        smdCat
        
        

        varnames <- c(varnames, label[i])
        varnames <- c(varnames, levels(data[,var[i]]))
        smd <- c(smd, format(round(smdCat,3), nsmall = 3), rep("", length(levels(data[,var[i]]))))
        
        
      }else {
        smd <- c(smd, "-")
        varnames <- c(varnames, label[i])}
    }
    
    if(MISSING == TRUE){
      if(any(is.na(data[,var[i]]))){
        varnames = c(varnames, "")
        smd=c(smd, "")
      }
    }

  }
  data.frame(variable=varnames, smd = smd)
}










###### TableDesc : to create the comparative table with a treatment and tests: ####

#data=object of class data.frame
#vars=list of variable names
#labels=list of labels for the variables
#trt= comparative group that needs to be specified else only a desc table for the total pop
#digits=list of 1: the number of decimals for continuous variables and 2: the number of decimals for percentages
#nolevels=vector of variables with 2 levels but transformed to 1 row (ex: Sexe, Homme Femme -> 1 Line Homme)
#nonnormal=NULL by default, simple Shapiro else "none": for every variables distributed normaly or "all": for every variables not distributed normaly or a list of variables not distributed normaly
#nrpval=number of decimals to be used for the pvalue
#boldpval=0.05 by default, puts all pvalues lower than set number in bold
#boldsmd=0.1 by default, puts all SMD upper than set number in bold
#reference=TRUE by default, any other value will remove the ", n (%)" after the label for categorical variables
#export = TRUE to export in a word document
#pvalue = TRUE to compare groups with statistical tests
#smd = TRUE to compute the standardized mean differences
#missing = TRUE to describe the number of missing values
#legend = TRUE to add a footnote
#paired = TRUE for two matched samples
#binary01 = TRUE if the binary variables are encoded as 0/1 


tableDESC = function(data,vars=NULL,labels=NULL,trt=NULL,digits=c(1,0),nolevels=NULL,nonnormal=NULL,nrpval=4,boldpval=0.05,boldsmd=0.1,reference=TRUE, export=FALSE,
                     pvalue=TRUE, smd = FALSE, missing = TRUE, legend=TRUE, paired=FALSE, binary01=FALSE) {
  data=droplevels(data)
  #If not data.frame but tibble:
  if (class(data)[1]!="data.frame") {
    tempcols=colnames(data)
    data=data.frame(data)
    colnames(data)=tempcols
  }
  #If trt is not factor it is set to a factor:
  if (!is.factor(data[,trt]) && !is.null(trt)) {
    data[,trt]=as.factor(data[,trt])
    warning(paste0(trt," was not set a factor, verify in the header that the reference levels are correct."))
  }
  
  if (is.null(vars)){
    if(!is.null(trt)){vars <- colnames(data)[-which(colnames(data) == trt)]}else{vars <- colnames(data)}}
  
  #Stop if any vars are not in the data
  if (any(vars %ni% names(data))) {stop(paste0("Some variables are not in the dataset: ",paste(vars[which(vars %ni% names(data))],collapse=" ; ")))}
  
  #if any vars is not numeric or factor put in numeric or factor
  for (i in vars) {
    if (binary01 == TRUE & is.numeric(data[,i]) & length(unique(data[,i])) == 2){
      data[,i] <- factor(data[,i], levels = c("1","0"))
    }
    if(!is.factor(data[,i]) & !is.numeric(data[,i])){data[,i] <- as.factor(data[,i])
    if(length(levels(data[,i])) >= 10){
      if(!any(class(tryCatch(as.numeric(as.character(data[,i])),error=function(e) e, warning=function(w) w)) == "warning")){
        data[,i] <- as.numeric(as.character(data[,i]))}}
    if(length(levels(data[,i]))> 20) {
      vars <- vars[-which(vars == i)]
      warning(paste0("Variable with >20 categories was not described. Put it in class factor if you want to describe it. For variable: ", i))
    }}
    
    if(i %in% nolevels){
      if(length(levels(data[,i])) != 2){nolevels <- nolevels[-which(nolevels == i)]}
    }}
  
  #Setting virg to the correct length:
  if (length(digits) == 1) {
    virg=virg.percent=digits
  } else if (length(digits) == 2){
    virg=digits[1]
    virg.percent=digits[2]
  } else{
    warning("Digits must have a length of 1 if the numeric and categorical variables are presented with the same number of decimal places, and a length of 2 otherwise.")
  }
  
  #If normal is empty then use checknormality function for each variable
  #OR if normal is not the same length as vars
  if (is.null(nonnormal)) {
    normal <- nonnormal
    for (i in 1:length(vars)) {
      if(length(unique(data[,vars[i]])) == 1){
        normal=c(normal,0)
      }else{
        normal=c(normal,checknormality(data=data,var=vars[i],TRT=trt))
      }
    }
  }else if ("none" %in% nonnormal){
    normal = rep(1, length(vars))
  }else if ("all" %in% nonnormal){
    normal = rep(0, length(vars))
  }else {
    normal = rep(1, length(vars))
    normal[which(vars %in% nonnormal)] <- 0
  }
  
  #If labels is not given the variable names are used as labels
  if (is.null(labels)) {labels=vars}
  
  #Use the FTdesc function to get the description of the general population
  tab=FTdesc(data=data,vars=vars,labels=labels,virg=virg,virg.percent=virg.percent,nolevels=nolevels,normal=normal,reference=reference, MISSING = missing)
  tab  
  colnames(tab)=c("",paste("All","\n(n=",dim(data)[1],")",sep=""))
  
  #If no trt is specified:
  if ((trt %ni% colnames(data)) && !is.null(trt) ) {trt=NULL} 
  if (is.null(trt)) {
    #Reporting with officer package
    Table=tab
    colnames(Table)[1]=" "
    Ftab=flextable(Table)
    Ftab=flextable::border_remove(Ftab)
    Ftab <- flextable::bold(Ftab, part = "header")
    font.name="Times New Roman"
    font.size=10
    Ftab <- flextable::font(Ftab,fontname=font.name,part ="all")
    Ftab <- flextable::fontsize(Ftab,size=font.size,part ="all")
    Ftab <- flextable::fontsize(Ftab,size=11,part ="header")
    Ftab <- flextable::align_text_col(Ftab, align = "left", header = TRUE)
    Ftab <- flextable::align(Ftab,i=1,j=2:ncol(Table),align = "center", part = "header")
    Ftab <- flextable::align(Ftab,i=1:nrow(Table),j=2:ncol(Table),align = "center", part = "body")
    Ftab <- flextable::hline(Ftab,border = fp_border(width = 1), part = "header" )
    Ftab <- flextable::hline_top(Ftab, border = fp_border(width = 1), part = "header" )
    Ftab <- flextable::hline_bottom(Ftab, border = fp_border(width = 1), part = "body")
    Ftab <- flextable::autofit(Ftab)
    Ftab <- flextable::height_all(Ftab, height=0.1, part = "body")
    Ftab <- flextable::height_all(Ftab, height=0.3, part = "header")
    Ftab <- flextable::height_all(Ftab, height=0.1, part = "footer")
    Ftab <- flextable::width(Ftab,j=1, width = 2.5)
    # Ftab <- width(Ftab,j=2:(ncol(Table)-1), width = 1.1) #no TRT thus only All column
    Ftab <- flextable::width(Ftab,j=ncol(Table), width = 1.1) #was 0.6
    
    boldrows=suppressWarnings(which(as.numeric(as.character(gsub("<","",Table[,ncol(Table)])))<boldpval))  #number of row to bold pvalue <0.05 by default
    
    for (rownr in boldrows)  {
      Ftab <- flextable::bold(Ftab,i=rownr,j=ncol(Table))
    }
    
    #To export and open a Word document
    if (isTRUE(export)) {
      doc <- read_docx()
      doc <- body_add_flextable(doc,value = Ftab)
      a=paste0(tempfile(),".docx")
      print(doc, target = a)
      # system(paste0("open",a))
      shell(a)
    }
    
    return(Ftab)
  }
  
  #Adding the values for the different groups
  for (trtgrp in names(table(data[,trt]))) {
    temp=c()
    
    for (i in 1:length(vars)) {
      if (is.factor(data[,vars[i]])) {
        
        #If in nolevels and 2 levels (OR 1 level meaning 100% answered)
        if (vars[i] %in% nolevels && length(levels(data[,vars[i]]))==2 ) {
          temp=c(temp,B=getresult(data=data[which(data[,trt]==trtgrp),],var=vars[i],levelj=levels(data[,vars[i]])[1],virg=virg,virg.percent=virg.percent))
        } else {
          #Add condition if only NA then return "-" 
          if (length(levels(data[which(data[,trt]==trtgrp),][,vars[i]]))==0) {
            #getresult will return "-" if only NA
            temp=c(temp,B=getresult(data=data[which(data[,trt]==trtgrp),],var=vars[i],levelj=levels(data[,vars[i]])[1],virg=virg,virg.percent=virg.percent))
          } else {
            #Add empty line and add result for each level
            temp=c(temp,B="")
            for (j in levels(data[,vars[i]])) {
              temp=c(temp,B=getresult(data=data[which(data[,trt]==trtgrp),],var=vars[i],levelj=j,virg=virg,virg.percent=virg.percent))
            }
          }
          
        }
        
      } else {
        #If numeric then simple getresult function
        temp=c(temp,B=getresult(data=data[which(data[,trt]==trtgrp),],var=vars[i],virg=virg,virg.percent=virg.percent,normal=normal[i]))
      }
      
      #if at least one missing value, add a row
    if(missing == TRUE){
      if(any(is.na(data[,vars[i]]))){
        temp=c(temp, sum(is.na(data[which(data[,trt]==trtgrp),vars[i]])*1))
      }
    }
    }
    
    
    
    tab=cbind(tab,temp)
  }
  
  
  
  test=temp=c()
  compt=0
  #Create progressbar
  pb <- txtProgressBar(min = 0, max = length(vars), style = 3)
  if(pvalue == TRUE){
    for (i in vars) {
    compt=compt+1
    if (is.factor(data[,i])) {
      
      #Check if any of the groups has only NA values, if so
      tempnotest=0
      for (j in 1:length(levels(data[,trt]))) {
        if (all(is.na(data[,i][which(data[,trt]==names(table(data[,trt]))[j])]))) {tempnotest=1}
      }
      
      #If one group only has NA values return "-":
      if (tempnotest==0) {
        temp=c(temp,comptest(data=data,var=i,grp=trt,nrpval=nrpval,paired=paired)[1])
        test=c(test,comptest(data=data,var=i,grp=trt,nrpval=nrpval,paired=paired)[2])
      } else { 
        temp=c(temp,"-")
        test = c(test,"-") }
      
      
      # If nolevels and 1 or 2 levels in total then 1 line:
      if (!(i %in% nolevels && (length(levels(data[,i]))==2 || length(levels(data[,i]))==1))) {
        for (j in levels(data[,i])) {
          temp=c(temp,"")
          test=c(test,"")
        }
      }
      
    } else {
      if (length(unique(data[,i])) > 1){
        temp=c(temp,comptest(data=data,var=i,grp=trt,normal=normal[compt],nrpval=nrpval,paired=paired)[1])
        test=c(test,comptest(data=data,var=i,grp=trt,normal=normal[compt],nrpval=nrpval,paired=paired)[2])
      }else{
        temp=c(temp,"-")
        test=c(test,"-")
      }
      
    }
    
      # If at least one missing value, add a row
  if(missing == TRUE){
    if(any(is.na(data[,i]))){
      temp=c(temp, "")
      test=c(test, "")
    }
  }
    # update progress bar
    setTxtProgressBar(pb, compt)
    }
    
    if (smd == TRUE) {
    tab=cbind(tab,temp,SMD(data=data,var=vars,TRT=trt,label=labels,nolevels=nolevels,MISSING=missing)[,2])
    headers=c()
    for (trtgrp in names(table(data[,trt]))) {
      headers=c(headers,paste0(trtgrp,"\n(n=",dim(data[which(data[,trt]==trtgrp),])[1],")"))
    }
    colnames(tab)=c("",paste("All","\n(n=",dim(data)[1],")",sep="")
                    ,headers,"p","SMD (%)")
  }else { 
    tab=cbind(tab,temp)
    headers=c()
    for (trtgrp in names(table(data[,trt]))) {
      headers=c(headers,paste0(trtgrp,"\n(n=",dim(data[which(data[,trt]==trtgrp),])[1],")"))
    }
    colnames(tab)=c("",paste("All","\n(n=",dim(data)[1],")",sep="")
                    ,headers,"p")
  }
    
  }else{
    
    if (smd == TRUE) {
      tab=cbind(tab,SMD(data=data,var=vars,TRT=trt,label=labels,nolevels=nolevels,MISSING=missing)[,2])
      headers=c()
      for (trtgrp in names(table(data[,trt]))) {
        headers=c(headers,paste0(trtgrp,"\n(n=",dim(data[which(data[,trt]==trtgrp),])[1],")"))
      }
      colnames(tab)=c("",paste("All","\n(n=",dim(data)[1],")",sep="")
                      ,headers,"SMD (%)")
    }else { 
      tab=cbind(tab)
      headers=c()
      for (trtgrp in names(table(data[,trt]))) {
        headers=c(headers,paste0(trtgrp,"\n(n=",dim(data[which(data[,trt]==trtgrp),])[1],")"))
      }
      colnames(tab)=c("",paste("All","\n(n=",dim(data)[1],")",sep="")
                      ,headers)
    }
    
  }
  
  

  
  #Close progress bar
  close(pb)
  
  temp
  test
  
  #Headers for the columns, to change the names you must directly change the level names of the trt variable
  
  
  
  
  
  #Reporting with officer package
  Table=tab
  colnames(Table)[1]=" "
  Ftab=flextable(Table)
  Ftab=flextable::border_remove(Ftab)
  Ftab <- flextable::bold(Ftab, part = "header")
  font.name="Calibri"
  font.size=10
  Ftab <- flextable::font(Ftab,fontname=font.name,part ="all")
  Ftab <- flextable::fontsize(Ftab,size=font.size,part ="all")
  Ftab <- flextable::fontsize(Ftab,size=11,part ="header")
  Ftab <- flextable::align_text_col(Ftab, align = "left", header = TRUE)
  Ftab <- flextable::align(Ftab,i=1,j=2:ncol(Table),align = "center", part = "header")
  Ftab <- flextable::align(Ftab,i=1:nrow(Table),j=2:ncol(Table),align = "center", part = "body")
  Ftab <- flextable::hline(Ftab,border = fp_border(width = 1), part = "header" )
  Ftab <- flextable::hline_top(Ftab, border = fp_border(width = 1), part = "header" )
  Ftab <- flextable::hline_bottom(Ftab, border = fp_border(width = 1), part = "body")
  Ftab <- flextable::autofit(Ftab)
  Ftab <- flextable::height_all(Ftab, height=0.1, part = "body")
  Ftab <- flextable::height_all(Ftab, height=0.3, part = "header")
  Ftab <- flextable::height_all(Ftab, height=0.1, part = "footer")
  Ftab <- flextable::width(Ftab,j=1, width = 2.5)
  Ftab <- flextable::width(Ftab,j=2:(ncol(Table)-1), width = 1.1)
  Ftab <- flextable::width(Ftab,j=ncol(Table), width = 0.6)
  Ftab <- flextable::italic(Ftab,i=which(Table[,1] == "Missing (n)"),italic = T)
  
  if(legend == TRUE){
    num=1
    if (smd == TRUE){
      Ftab <- flextable::footnote(Ftab,i = 1, j = which(colnames(Table) == "SMD (%)"), 
                                  part = "header", inline = TRUE,
                                  ref_symbols = letters[num], value = as_paragraph("SMD: standardized mean difference"))
      Ftab <- flextable::italic(Ftab,italic = T,part = "footer")
      Ftab <- flextable::fontsize(Ftab, size = 9, part = "footer")
      num=num+1
    }
    if (pvalue == TRUE){
      
      for (i in 1:length(unique(test))) {
        if(unique(test)[i] %ni% c("-","")){
          Ftab <- flextable::footnote(Ftab,i = which(test == unique(test)[i]), 
                                      j = which(colnames(Table) == "p"), 
                                      inline = TRUE,
                                  ref_symbols = letters[num], value = as_paragraph(unique(test)[i]))
          Ftab <- flextable::italic(Ftab,italic = T,part = "footer")
          Ftab <- flextable::fontsize(Ftab, size = 9, part = "footer")
          num=num+1
        }
      }
      
    }
    }
  
  
  if (smd == TRUE ){
    boldrows=suppressWarnings(which(as.numeric(as.character(gsub("<","",Table[,ncol(Table)-1])))<boldpval))
    boldrows2=suppressWarnings(which(as.numeric(as.character(gsub("<","",Table[,ncol(Table)])))>boldsmd)) #number of row to bold SMD > 10%
  } else {
    boldrows=suppressWarnings(which(as.numeric(as.character(gsub("<","",Table[,ncol(Table)])))<boldpval))
  }
  #number of row to bold pvalue <0.05 by default
  
  if (smd == TRUE){ 
    for (rownr in boldrows)  {
      Ftab <- flextable::bold(Ftab,i=rownr,j=(ncol(Table)-1))
    }
    for (rownr in boldrows2)  {
      Ftab <- flextable::bold(Ftab,i=rownr,j=(ncol(Table)))
    }
  }else {
    for (rownr in boldrows)  {
      Ftab <- flextable::bold(Ftab,i=rownr,j=(ncol(Table)))
    }
  }
  
  
  #To export and open a Word document
  if (isTRUE(export)) {
    doc <- read_docx()
    doc <- body_add_flextable(doc,value = Ftab)
    a=paste0(tempfile(),".docx")
    print(doc, target = a)
    # system(paste0("open",a))
    shell(a)
  }
  
  return(Ftab)
  
}


