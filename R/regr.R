PATTERNS = c('digit' = '[0-9]',
             'lowercase' = '[a-z]',
             'uppercase' = '[A-Z]',
             'letter' = '[A-Za-z]',
             'alphanumeric' = '[A-Za-z0-9]',
             'whitespace' = '\\s',
             'space' = ' ',
             'tab' = '\t')

# Remove any trailing 's'
SingularisePattern <- function(pattern){
    if(substr(pattern, nchar(pattern), nchar(pattern)) == 's'){
        return(substr(pattern, 0, nchar(pattern) - 1))
    }else{
        return(pattern)
    }
}

# Escape the new pattern
EscapePattern <- function(pattern){
    return(gsub("([.|()\\^{}+$*?])", "\\\\\\1", pattern))
}

# Get the actual pattern to be used
Translate <- function(pattern){
    tmp <- PATTERNS[which(names(PATTERNS) == SingularisePattern(pattern))]
    if(length(tmp) == 0){
        return(pattern)
    }else if(length(tmp) == 1){
        return(tmp)
    }else{
        stop('Pattern error detected in regr')
    }
}

# Do we have a numbered pattern??
Interpret <- function(...){
    args <- as.list(...)
    if(length(args) == 1){
        return(PatternedConstraint(args[1]))
    }else if(length(args) == 2){
        return(NumberedConstraint(args[1], args[2]))
    }else{
        stop('regr Interpret received incorrect number of parameters')
    }
}

PatternedConstraint <- function(type){
    return(Translate(EscapePattern(type)))
}

NumberedConstraint <- function(count, type){
    pattern <- PatternedConstraint(type)
    pattern <- paste0(c(pattern, '{', as.character(count), '}'), collapse='')
    return(pattern)
}

Write <- function(regr_object, pattern){
    regr_object$regex <- paste0(regr_object$regex, pattern, collapse='')
    return(regr_object)
}

#' Starts a new regr object
#' @export
#' @examples
#' regr()
regr <- function(){
    tmp <- list()
    tmp$regex <- ''
    class(tmp) <- 'regr'
    tmp$started <- FALSE
    tmp$ended <- FALSE
    return(tmp)
}


#' The line must start with the specified pattern
#'
#' @param regr_object created from regr
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits')
StartWith <- function(regr_object, ...){
    # check the incoming regr object is valid
    if(class(regr_object) != 'regr'){
         stop('Error, requries regr object')
    }else if(regr_object$started == TRUE){
        stop('Error, StartsWith must be called on regr before other arguments')
    }else if(regr_object$ended == TRUE){
        stop('Error, regr_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    regr_object$started <- TRUE
    pattern <- Interpret(list(...))
    regr_object <- Write(regr_object, '^')
    return(Write(regr_object, pattern))
}

#' Append a pattern to the end (Also aliased to then)
#'
#' @param regr_object created from regr
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits') %>% Then('-')
Then <- function(regr_object, ...){
    # check the incoming regr object is valid
    if(class(regr_object) != 'regr'){
        stop('Error, requries regr object')
    }else if(regr_object$ended == TRUE){
        stop('Error, regr_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    regr_object$started <- TRUE
    pattern <- Interpret(list(...))
    return(Write(regr_object, pattern))
}


#' Append a pattern to the end (Also aliased to Append)
#'
#' @param regr_object created from regr
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits') %>% Append('-')
Append <- function(regr_object, ...){
    return(Then(regr_object, ...))
}

#' Zero or one of the specified pattern
#'
#' @param regr_object created from regr
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits') %>% Maybe('digit')
Maybe <- function(regr_object, ...){
    # check the incoming regr object is valid
    if(class(regr_object) != 'regr'){
        stop('Error, requries regr object')
    }else if(regr_object$ended == TRUE){
        stop('Error, regr_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    regr_object$started <- TRUE
    pattern <- Interpret(list(...))
    regr_object <- Write(regr_object, '(')
    regr_object <- Write(regr_object, pattern)
    regr_object <- Write(regr_object, ')')
    return(Write(regr_object, '?'))
}


#' Specify a bounded repetition, e.g. between(c(2,4), 'digits')
#'
#' @param regr_object created from regr
#' @param range vector containing the upper and lower bound e.g. c(1, 5)
#' @param pattern can be either a regex pattern or count and pattern
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits') %>% Between(c(1, 2), 'a') 
Between <- function(regr_object, range, pattern){
    # check the incoming regr object is valid
    if(class(regr_object) != 'regr'){
        stop('Error, requries regr object')
    }else if(regr_object$ended == TRUE){
        stop('Error, regr_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    regr_object$started <- TRUE
    pattern <- Interpret(list(pattern))
    regr_object <- Write(regr_object, pattern)
    return(Write(regr_object, paste0('{', as.character(range[1]), ',', 
                                     as.character(range[2]), '}')))
}


#' Specify that the pattern or identifer should appear n or more times
#'
#' @param regr_object created from regr
#' @param count hpow many times the pattern shouold repeat
#' @param pattern can be either a regex pattern or count and pattern
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits') %>% AtLeast(5, 'a') 
AtLeast <- function(regr_object, count, pattern){
    # check the incoming regr object is valid
    if(class(regr_object) != 'regr'){
        stop('Error, requries regr object')
    }else if(regr_object$ended == TRUE){
        stop('Error, regr_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    regr_object$started <- TRUE
    pattern <- Interpret(list(pattern))
    regr_object <- Write(regr_object, pattern)
    return(Write(regr_object, paste0('{', as.character(count), ',', '}')))
}


#' The line must end with the specified pattern. Note that the regr
#' objext cannot be modifed once EndWith has been called on it
#'
#' @param regr_object created from regr
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits') %>% EndWith('>') 
EndWith <- function(regr_object, ...){
    # check the incoming regr object is valid
    if(class(regr_object) != 'regr'){
        stop('Error, requries regr object')
    }else if(regr_object$ended == TRUE){
        stop('Error, regr_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    regr_object$started <- TRUE
    regr_object$ended <- TRUE
    pattern <- Interpret(list(...))
    regr_object <- Write(regr_object, pattern)
    return(Write(regr_object, '$'))
}


#' Specify an alternation, e.g. one_of c('a', 'b', 'c')
#'
#' @param regr_object created from regr
#' @param pattern the pattern to be matched
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits') %>% OneOf(c('a', 'b', 'c')) 
OneOf <- function(regr_object, pattern){
    # check the incoming regr object is valid
    if(class(regr_object) != 'regr'){
        stop('Error, requries regr object')
    }else if(regr_object$ended == TRUE){
        stop('Error, regr_object has been ended using EndWith')
    }
    regr_object$started <- TRUE
    pattern <- sapply(pattern, Translate)
    return(Write(regr_object, paste0('(', paste0(pattern, 
                                                 collapse='|'), ')')))
}


#' Specify that the pattern or identifer should appear zero or many times
#'
#' @param regr_object created from regr
#' @param pattern the pattern to be matched
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits') %>% OneOf('a') 
ZeroOrMore <- function(regr_object, pattern){
    # check the incoming regr object is valid
    if(class(regr_object) != 'regr'){
        stop('Error, requries regr object')
    }else if(regr_object$ended == TRUE){
        stop('Error, regr_object has been ended using EndWith')
    }
    regr_object$started <- TRUE
    pattern <- Interpret(list(pattern))
    regr_object <- Write(regr_object, '(')
    regr_object <- Write(regr_object, pattern)
    regr_object <- Write(regr_object, ')')
    return(Write(regr_object, '*'))
}


#' Specify that the pattern or identifer should appear one or many times
#'
#' @param regr_object created from regr
#' @param pattern the pattern to be matched
#' @export
#' @return a regr object
#' @examples
#' regr() %>% StartWith(3, 'digits') %>% OneOrMore('a') 
OneOrMore <- function(regr_object, pattern){
    # check the incoming regr object is valid
    if(class(regr_object) != 'regr'){
        stop('Error, requries regr object')
    }else if(regr_object$ended == TRUE){
        stop('Error, regr_object has been ended using EndWith')
    }
    regr_object$started <- TRUE
    pattern <- Interpret(pattern)
    regr_object <- Write(regr_object, pattern)
    return(Write(regr_object, '+'))
}
