.packageName <- "formula_utils"

##' @title Return the variables in a formula as a character list.
##' @param form a formula object.
##' @param strip if TRUE then transformation functions will be removed from variable names.
##' @return Character vector containing names of all variables in formula 'form'. 
##' The first element in the vector corresponds to the right hand side variable of 'form'.
##' @author Ben Veal
##' @export
get.vars2 <- function(form,strip=FALSE)
{
  lhs <- lhs.vars(form)
  rhs <- rhs.vars(form)
  if(class(lhs)[1]=="character")
    vars <- c(lhs[1],rhs)
  else if(class(lhs)[1]=="terms")
    vars <- c(attr(lhs[1],"term.labels")[1],rhs)
  else if(class(lhs)[1]=="NULL")
    vars <- rhs
  if(strip)
    return(sub(").*","",c(sub(".*\\(","",vars))))
  else
    return(vars)
}


##' @title Create a formula from a list of variables and corresponding transformations.
##' @details The "transforms" argument should be a list with elements corresponding to variables in the "vars" argument.
##' An element of "transforms" can be an empty string, in which case the corresponding variable is left unchanged,
##' or a string containing a ? symbol, in which case the ? will be replaced by the corresponding variable name, e.g.
##' ?+1 -> VAR+1. The "transforms" list may also contain named argument where the name can be one of 'lags', 'Lags' or
##' 'diffs'. If the element is named 'lags' or 'Lags' its value should be a numeric vector indicating which lags of the
##' corresponding variable to include, e.g. lags=1:3 -> lag(VAR,1) + lag(VAR,2) + lag(VAR,3).
##' If the element is named 'diffs' then its value can either be a numeric vector, in which case differences of the
##' given orders will be included, or a list of two numeric vectors. In the latter case the first numeric vector indicates
##' the lags to use for the differences and the second vector indicates the orders of differences to use. All different
##' combinations of these lags and orders will be used in the difference operator applied to the corresponding variable,
##' e.g: diffs=list(1:2,3:4) -> diff(VAR,lag=1,differences=3) + diff(VAR,lag=2,differences=3)
##' + diff(VAR,lag=1,differences=4) + diff(VAR,lag=2,differences=4)
##' If length(transforms) < length(vars) then transforms will be padded with empty strings (which will be replaced by
##' the corresponding elements of vars in the formula).
##' @param vars character vector containing the names of the variables (before transformation) in the formula.
##' The first element should be the lhs variable, and the following elements are the rhs variables.
##' @param transforms list of transformations to be applied to corresponding variables in vars.
##' See the details.
##' @return A formula object
##' @author Ben Veal
##' @examples MakeFormula(c("y","x","z","w"),list("",lags=1:3,diffs=list(3:4,1:2),"?*2"))
##' @export
MakeFormula <- function(vars,transforms="")
{
  stopifnot(class(vars)=="character" && length(vars) > 1)
  stopifnot(class(transforms)=="list")
  nvars <- length(vars)
  ntrans <- length(transforms)
  if(nvars < ntrans)
    warning("More transforms than vars. Only the first length(vars) of them will be used.")
  else if(ntrans < nvars)
    {
      warning("There are more vars than transforms. transforms will be padded with empty strings to match the extra vars.")
      transforms[(ntrans+1):nvars] <- ""
    }
  terms <- ""
  for(i in 1:length(vars))
    {
      var <- vars[i]
      trans <- transforms[[i]]
      type <- names(transforms)[i]
      if(type=="lags")
        {
          stopifnot(class(trans) %in% c("numeric","integer"))
          terms <- paste(terms,"+",paste(sub("VAR",var,paste("lag(VAR,",trans,")")),collapse="+"))
        }
      else if(type=="Lags")
        {
          stopifnot(class(trans) %in% c("numeric","integer"))          
          terms <- paste(terms,"+",paste(sub("VAR",var,paste("Lag(VAR,",trans,")")),collapse="+"))
        }
      else if(type=="diffs")
        {
          stopifnot(class(trans) %in% c("numeric","integer","list"))
          if(class(trans) %in% c("numeric","integer"))
            terms <- paste(terms,"+",paste(sub("VAR",var,paste("diff(VAR,differences=",trans,")")),collapse="+"))
          else
            {
              stopifnot(length(trans)==2)
              stopifnot(class(trans[[1]]) %in% c("numeric","integer"))
              stopifnot(class(trans[[2]]) %in% c("numeric","integer"))
              lags <- sub("VAR",var,paste("diff(VAR,lags=",trans[[1]]))
              for(lag in lags)
                terms <- paste(terms,"+",paste(lag,",differences=",trans[[2]],")",collapse="+"))
            }
        }
      else if(type=="")
        {
          stopifnot(class(trans)=="character")
          if(trans=="")
            terms <- paste(terms,"+",var)
          else
            terms <- paste(terms,"+",sub("\\?",var,trans))
        }
    }
  terms <- sub("\\+","",terms)
  terms <- sub("\\+","~",terms)
  return(as.formula(terms))
}

##' @title Create formula from union of independent variables of other formulae.
##' @details The first formula/string argument should be either the name of the dependent variable or a formula
##' object or string representation of a formula containing a dependent variable (and a ~).
##' This will be used as the dependent variable of the result. The independent variables of the returned result will
##' consist of all independent variables in the function arguments without repetitions.
##' @param ... Formula objects or strings representing formulas or their right hand sides.
##' @param asString If TRUE then return formula as a string, otherwise as a formula object.
##' @return A formula object or a string representing a formula object.
##' @author Ben Veal
##' @export
## NOTE : NEED TO ALTER THIS SO THAT I CAN ALSO PASS THE NAME OF THE DEPENDENT AS THE FIRST ARG
funion <- function(...,asString=FALSE)
{
  parts <- as.list(unlist(list(...)))
  part1 <- parts[[1]]
  stopifnot(any(c("formula","character") %in% class(part1)))
  subparts <- character()
  if("formula" %in% class(part1))
    lhs <- as.character(part1)[2]
  else
    {
      lhs <- trim(strsplit(part1,"~")[[1]][1])
      if(length(grep("~",part1)>0))
        subparts <- trim(unlist(strsplit(strsplit(part1,"~")[[1]][2],"\\+")))
    }
  for(i in 2:length(parts))
  {
    part <- parts[[i]]
    stopifnot(any(c("formula","character") %in% class(part)))
    if("formula" %in% class(part))
      subparts <- union(subparts,trim(unlist(strsplit(as.character(part)[3],"\\+"))))
    else
      {
        if(length(grep("~",part)>0))
          part <- strsplit(part,"~")[[1]][2]
        subparts <- union(subparts,trim(unlist(strsplit(part,"\\+"))))
      }
  }
  subparts <- na.omit(subparts)
  if(length(subparts) > 0)
    rhs <- paste(subparts,collapse=" + ")
  else
    rhs <- "1"
  form <- paste(lhs,rhs,sep="~")
  if(asString)
    return(form)
  else
    return(as.formula(form))
}

##' @title Create formula from intersection of independent variables of other formulae.
##' @details The first formula/string argument should be either the name of the dependent variable or a formula object or
##' string representation of a formula containing a dependent variable (and a ~).
##' This will be used as the dependent variable of the result. The independent variables of the returned result will
##' consist the intersection of all independent variables in the function arguments.
##' @param ... Formula objects or strings representing formulas or their right hand sides.
##' @param asString If TRUE then return formula as a string, otherwise as a formula object.
##' @return A formula object or a string representing a formula object.
##' @author Ben Veal
##' @export 
fintersect <- function(...,asString=FALSE)
{
  parts <- as.list(unlist(list(...)))
  part1 <- parts[[1]]
  stopifnot(any(c("formula","character") %in% class(part1)))
  subparts <- character()
  if("formula" %in% class(part1))
    lhs <- as.character(part1)[2]
  else
    {
      lhs <- trim(strsplit(part1,"~")[[1]][1])
      if(length(grep("~",part1)>0))
        subparts <- trim(unlist(strsplit(strsplit(part1,"~")[[1]][2],"\\+")))
    }
  for(i in 2:length(parts))
  {
    part <- parts[[i]]
    stopifnot(any(c("formula","character") %in% class(part)))
    if("formula" %in% class(part))
      subparts <- intersect(subparts,trim(unlist(strsplit(as.character(part)[3],"\\+"))))
    else
      {
        if(length(grep("~",part)>0))
          part <- strsplit(part,"~")[[1]][2]
        subparts <- intersect(subparts,trim(unlist(strsplit(part,"\\+"))))
      }
  }
  subparts <- na.omit(subparts)
  if(length(subparts) > 0)
    rhs <- paste(subparts,collapse=" + ")
  else
    rhs <- "1"
  form <- paste(lhs,rhs,sep="~")
  if(asString)
    return(form)
  else
    return(as.formula(form))
}

##' @title Remove common independent variables from formulas.
##' @details The first formula/string argument should be either the name of the dependent variable or a formula object or
##' string representation of a formula containing a dependent variable (and a ~).
##' This will be used as the dependent variable of the result.
##' By default any independent variables that are in ALL the formula/string arguments to the function are removed
##' from each of those formulas and the results returned as either a list of formula arguments (asString=FALSE, default),
##' or a character vector of strings representing those formulas (asString=TRUE).
##' If diffUnion==TRUE then instead all independent variables that are in ANY of the formula arguments will be removed.
##' @param ... Formula objects or strings representing formulas or their right hand sides.
##' @param asString If TRUE then return formula as a string, otherwise as a formula object.
##' @param diffUnion If TRUE then remove from each formula any terms that are in ANY of the other formulas
##' (otherwise only terms that are in ALL of the other formulas are removed).
##' @return A formula object or a string representing a formula object.
##' @author Ben Veal
##' @export 
fsetdiff <- function(...,asString=FALSE,diffUnion=FALSE)
{
  parts <- as.list(unlist(list(...)))
  part1 <- parts[[1]]
  stopifnot(any(c("formula","character") %in% class(part1)))
  if("formula" %in% class(part1))
    lhs <- as.character(part1)[2]
  else
    lhs <- trim(strsplit(part1,"~")[[1]][1])
  if(!diffUnion)
    diff <- rhs.vars(fintersect(...,asString=FALSE))
  newparts <- list()
  for(i in 1:length(parts))
  {
    part <- parts[[i]]
    stopifnot(any(c("formula","character") %in% class(part)))
    if(diffUnion)
      diff <- rhs.vars(funion(parts[-i]))
    if("formula" %in% class(part))
      {
        rhsvars <- setdiff(trim(unlist(strsplit(as.character(part)[3],"\\+"))),diff)
      }
    else
      {
        if(length(grep("~",part)>0))
          part <- strsplit(part,"~")[[1]][2]
        if(!is.na(part) && length(part) > 0)
          {
            rhsvars <- setdiff(trim(unlist(strsplit(part,"\\+"))),diff)
            if(length(rhsvars) == 0)
              rhsvars <- "1"
          }
        else
          rhsvars <- "1"
      }
    newparts[[i]] <- paste(lhs,paste(rhsvars,collapse=" \\+ "),sep=" ~ ")
  }
  if(!asString)
    return(lapply(newparts,as.formula))
  else
    return(unlist(newparts))
}


