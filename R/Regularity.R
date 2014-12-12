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
        stop('Pattern error detected in Regularity')
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
        stop('Regularity Interpret received incorrect number of parameters')
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

Write <- function(Regularity_object, pattern){
    Regularity_object$regex <- paste0(Regularity_object$regex, pattern, collapse='')
    return(Regularity_object)
}

#' Starts a new Regularity object
#' @export
#' @examples
#' Regularity()
Regularity <- function(){
    tmp <- list()
    tmp$regex <- ''
    class(tmp) <- 'Regularity'
    tmp$started <- FALSE
    tmp$ended <- FALSE
    return(tmp)
}


#' The line must start with the specified pattern
#'
#' @param Regularity_object created from Regularity
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits')
StartWith <- function(Regularity_object, ...){
    # check the incoming Regularity object is valid
    if(class(Regularity_object) != 'Regularity'){
         stop('Error, requries Regularity object')
    }else if(Regularity_object$started == TRUE){
        stop('Error, StartsWith must be called on Regularity before other arguments')
    }else if(Regularity_object$ended == TRUE){
        stop('Error, Regularity_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    Regularity_object$started <- TRUE
    pattern <- Interpret(list(...))
    Regularity_object <- Write(Regularity_object, '^')
    return(Write(Regularity_object, pattern))
}

#' The line must start with the specified pattern (aliased to StartWith function)
#'
#' @param Regularity_object created from Regularity
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits')
StartsWith <- function(Regularity_object, ...){
  return(StartWith(Regularity_object, ...))
}

#' Append a pattern to the end (Also aliased to then)
#'
#' @param Regularity_object created from Regularity
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits') %>% Then('-')
Then <- function(Regularity_object, ...){
    # check the incoming Regularity object is valid
    if(class(Regularity_object) != 'Regularity'){
        stop('Error, requries Regularity object')
    }else if(Regularity_object$ended == TRUE){
        stop('Error, Regularity_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    Regularity_object$started <- TRUE
    pattern <- Interpret(list(...))
    return(Write(Regularity_object, pattern))
}


#' Append a pattern to the end (Also aliased to Append)
#'
#' @param Regularity_object created from Regularity
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits') %>% Append('-')
Append <- function(Regularity_object, ...){
    return(Then(Regularity_object, ...))
}

#' Zero or one of the specified pattern
#'
#' @param Regularity_object created from Regularity
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits') %>% Maybe('digit')
Maybe <- function(Regularity_object, ...){
    # check the incoming Regularity object is valid
    if(class(Regularity_object) != 'Regularity'){
        stop('Error, requries Regularity object')
    }else if(Regularity_object$ended == TRUE){
        stop('Error, Regularity_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    Regularity_object$started <- TRUE
    pattern <- Interpret(list(...))
    Regularity_object <- Write(Regularity_object, '(')
    Regularity_object <- Write(Regularity_object, pattern)
    Regularity_object <- Write(Regularity_object, ')')
    return(Write(Regularity_object, '?'))
}


#' Specify a bounded repetition, e.g. between(c(2,4), 'digits')
#'
#' @param Regularity_object created from Regularity
#' @param range vector containing the upper and lower bound e.g. c(1, 5)
#' @param pattern can be either a regex pattern or count and pattern
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits') %>% Between(c(1, 2), 'a') 
Between <- function(Regularity_object, range, pattern){
    # check the incoming Regularity object is valid
    if(class(Regularity_object) != 'Regularity'){
        stop('Error, requries Regularity object')
    }else if(Regularity_object$ended == TRUE){
        stop('Error, Regularity_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    Regularity_object$started <- TRUE
    pattern <- Interpret(list(pattern))
    Regularity_object <- Write(Regularity_object, pattern)
    return(Write(Regularity_object, paste0('{', as.character(range[1]), ',', 
                                     as.character(range[2]), '}')))
}


#' Specify that the pattern or identifer should appear n or more times
#'
#' @param Regularity_object created from Regularity
#' @param count hpow many times the pattern shouold repeat
#' @param pattern can be either a regex pattern or count and pattern
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits') %>% AtLeast(5, 'a') 
AtLeast <- function(Regularity_object, count, pattern){
    # check the incoming Regularity object is valid
    if(class(Regularity_object) != 'Regularity'){
        stop('Error, requries Regularity object')
    }else if(Regularity_object$ended == TRUE){
        stop('Error, Regularity_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    Regularity_object$started <- TRUE
    pattern <- Interpret(list(pattern))
    Regularity_object <- Write(Regularity_object, pattern)
    return(Write(Regularity_object, paste0('{', as.character(count), ',', '}')))
}


#' The line must end with the specified pattern. Note that the Regularity
#' objext cannot be modifed once EndWith has been called on it
#'
#' @param Regularity_object created from Regularity
#' @param ... can be either a regex pattern or count and pattern
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits') %>% EndWith('>') 
EndWith <- function(Regularity_object, ...){
    # check the incoming Regularity object is valid
    if(class(Regularity_object) != 'Regularity'){
        stop('Error, requries Regularity object')
    }else if(Regularity_object$ended == TRUE){
        stop('Error, Regularity_object has been ended using EndWith')
    }
    # set the flag for StartsWith
    Regularity_object$started <- TRUE
    Regularity_object$ended <- TRUE
    pattern <- Interpret(list(...))
    Regularity_object <- Write(Regularity_object, pattern)
    return(Write(Regularity_object, '$'))
}


#' Specify an alternation, e.g. one_of c('a', 'b', 'c')
#'
#' @param Regularity_object created from Regularity
#' @param pattern the pattern to be matched
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits') %>% OneOf(c('a', 'b', 'c')) 
OneOf <- function(Regularity_object, pattern){
    # check the incoming Regularity object is valid
    if(class(Regularity_object) != 'Regularity'){
        stop('Error, requries Regularity object')
    }else if(Regularity_object$ended == TRUE){
        stop('Error, Regularity_object has been ended using EndWith')
    }
    Regularity_object$started <- TRUE
    pattern <- sapply(pattern, Translate)
    return(Write(Regularity_object, paste0('(', paste0(pattern, 
                                                 collapse='|'), ')')))
}


#' Specify that the pattern or identifer should appear zero or many times
#'
#' @param Regularity_object created from Regularity
#' @param pattern the pattern to be matched
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits') %>% OneOf('a') 
ZeroOrMore <- function(Regularity_object, pattern){
    # check the incoming Regularity object is valid
    if(class(Regularity_object) != 'Regularity'){
        stop('Error, requries Regularity object')
    }else if(Regularity_object$ended == TRUE){
        stop('Error, Regularity_object has been ended using EndWith')
    }
    Regularity_object$started <- TRUE
    pattern <- Interpret(list(pattern))
    Regularity_object <- Write(Regularity_object, '(')
    Regularity_object <- Write(Regularity_object, pattern)
    Regularity_object <- Write(Regularity_object, ')')
    return(Write(Regularity_object, '*'))
}


#' Specify that the pattern or identifer should appear one or many times
#'
#' @param Regularity_object created from Regularity
#' @param pattern the pattern to be matched
#' @export
#' @return a Regularity object
#' @examples
#' Regularity() %>% StartWith(3, 'digits') %>% OneOrMore('a') 
OneOrMore <- function(Regularity_object, pattern){
    # check the incoming Regularity object is valid
    if(class(Regularity_object) != 'Regularity'){
        stop('Error, requries Regularity object')
    }else if(Regularity_object$ended == TRUE){
        stop('Error, Regularity_object has been ended using EndWith')
    }
    Regularity_object$started <- TRUE
    pattern <- Interpret(pattern)
    Regularity_object <- Write(Regularity_object, pattern)
    return(Write(Regularity_object, '+'))
}
