#' add a rule to a strategy
#' 
#' Rules will be processed in a very particular manner, so it bears going over.
#' 
#' First, rules are either path dependent or non-path-dependent.  Path dependent rules 
#' will be processed in every time increment for the \code{mktdata} passed into
#' \code{\link{applyStrategy}}.  Non path dependent rules will likely be quite rare in real life, 
#' and will be applied after indicators and signals, and before path-dependent rules are processed.
#' 
#' 
#' All rules have a \code{type}.  These may be any of:
#' \describe{
#'   \item{risk}{ rules that check and react to risk of positions, may stop all other rule execution temporarily or permanently}
#'   \item{order}{ rules for order processing of any open orders at time t, always path-dependent}
#'   \item{rebalance}{ rules executed specifically in a portfolio context, unnecessary in univariate strategies}
#'   \item{exit}{ rules to determine whether to exit a position}
#'   \item{enter}{ rules to determine whether to enter or increase a position}
#'   \item{chain}{ rules executed upon fill of an order corresponding to the label of the parent rule identified by the \code{parent} arg. }
#' } 
#'  
#' The rules will be executed by type, in the order listed above.  
#' Multiple rules of each type may be defined, as with signals and indicators, 
#' they will be executed in order by index number with any other rules sharing the same 
#' type.
#' 
#' The rule execution order was constructed because path-dependent rules may modify   
#' the ability of rules that have not fired yet to be evaluated.  For example, a 
#' risk rule may flatten (close out) an entire position and put new orders 
#' on hold, effectively stopping all further execution of the strategy.  
#' Another example would be a rebalancing rule function that would enter 
#' orders to rebalance the portfolio, and would hold other strategy processing 
#' until the rebalancing period was over.
#' 
#' The \code{timespan} parameter will limit rule execution by time of day using 
#' time based subsetting.  See ISO-8601 specification and xts documentation for 
#' more details.  Note that these are only applicable to intra-day execution, 
#' and will remain that way barring patches (tests and documentation) from 
#' interested parties.  The subsetting may (will likely) work with normal 
#' ISO/xts subset ranges, but consider it unsupported. 
#' 
#' The \code{name} parameter should be a character string naming the function
#' to be called in the \code{\link{applyRules}} loop. The \code{add.rule} 
#' function will then call \code{\link{match.fun}}, ands store the actual function 
#' in your strategy object.  
#' This will avoid lookups via \code{\link{match.fun}} at \code{\link{applyRules}} time, 
#' and may provide a significant speed increase on higher frequency data (20\% or more).
#' 
#' We anticipate that rules will be the portion of a strategy most likely to 
#' not have suitable template code included with this package, as every strategy 
#' and environment are different, especially in this respect.  
#' We will attempt to provide enough examples and generic rules to give strategy
#' authors a place to start.
#' 
#' For quantstrat to be able to (largly) vectorize the execution of path-dependent 
#' rule evaluation, the rule function is presumed to have a function signature 
#' like that of \code{\link{ruleSignal}}, specifically the arguments \code{sigcol} 
#' and \code{sigval}.  If these are present and function in a manner similar to 
#' \code{\link{ruleSignal}} we can do some preprocessing to significantly reduce the 
#' dimensionality of the index we need to loop over.  The speedup is the ratio of 
#' (symbols\*total observations)/signal observations, so it can be significant for many strategies.
#' 
#' @param strategy an object of type 'strategy' to add the rule to
#' @param name name of the rule, must correspond to an R function
#' @param arguments named list of default arguments to be passed to an rule function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition
#' @param label arbitrary text label for rule output, NULL default will be converted to '<name>.rule'
#' @param type one of "risk","order","rebalance","exit","enter","chain" see Details
#' @param parent the label of the parent rule for a chain rule
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific rule, the index number in the $rules[type] list to update
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details 
#' @param timespan an xts/ISO-8601 style \emph{time} subset, like "T08:00/T15:00", see Details
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @param storefun TRUE/FALSE whether to store the function in the rule, default TRUE.  setting this option to FALSE may slow the backtest, but makes \code{\link{debug}} usable
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy. 
#' @export
add.rule <- function(strategy
                     , name
                     , arguments
                     , parameters=NULL
                     , label=NULL
                     , type=c(NULL,"risk","order","rebalance","exit","enter","chain")
                     , parent=NULL
                     , ...
                     , enabled=TRUE
                     , indexnum=NULL
                     , path.dep=TRUE
                     , timespan=NULL
                     , store=FALSE
                     , storefun=TRUE) {
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE
    } 
    type=type[1]
    if(is.null(type)) stop("You must specify a type")
    if(is.na(charmatch(type,c("risk","order","rebalance","exit","enter","chain","pre","post")))) stop(paste("type:",type,' must be one of "risk", "order", "rebalance", "exit", "enter", "chain", "pre", or "post"'))
    tmp_rule<-list()
    if(!is.function(name) && isTRUE(storefun)) {
        if(exists(name, mode="function")) {
            fn <- get(name, mode="function")
        } else {
            rule.name <- paste("rule", name, sep=".")
            if(exists(rule.name, mode="function")) {
                fn <- get(rule.name, mode="function")
                name <- rule.name
            } else {
                message("Skipping rule ", name,
                        " because there is no function by that name to call")
            }
        }
    } else {
        fn <- name
    }

    tmp_rule$name<-fn
    tmp_rule$type<-type
    if(type == 'chain')
    {
        if(is.null(parent)) stop("You must specify the label of the parent rule if ruletype=='chain'")
        tmp_rule$parent<-parent
    }
    tmp_rule$enabled<-enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
    if(is.null(label)) label = paste(name,"rule",sep='.')
    tmp_rule$label<-label
    tmp_rule$arguments<-arguments
    if(!is.null(parameters)) tmp_rule$parameters = parameters
    if(!is.null(timespan)) tmp_rule$timespan = timespan
    tmp_rule$path.dep<-path.dep
    if(length(list(...))) tmp_rule<-c(tmp_rule,list(...))

    tmp_rule$call<-match.call()
    class(tmp_rule)<-'trade_rule'
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$rules[[type]])+1
    strategy$rules[[type]][[indexnum]]<-tmp_rule

    #increment trials
    strategy$trials <- strategy$trials+1
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
    strategy$name
}

#' enable a rule in the strategy
#'
#' function to make it easy to enable (or disable) a specific rule in a strategy
#'
#' @param strategy an object of type 'strategy' which contains the rule
#' @param type one of "risk","order","rebalance","exit","enter","chain"
#' @param label the label for the rule; grep will be used to match, so multiple rules may be enabled (disabled) as a result
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param store TRUE/FALSE whether to store the updated strategy in the .strategy environment, or return it.  default FALSE
#' @seealso \code{\link{add.rule}} \code{\link{applyStrategy}} 
#' @export

enable.rule <- function(strategy, type=c(NULL,"risk","order","rebalance","exit","enter","chain"), label, enabled=TRUE, store=FALSE)
{
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE
    } 

    for(i in 1:length(strategy$rules[[type]]))
        if(grepl(label, strategy$rules[[type]][[i]]$label))
            strategy$rules[[type]][[i]]$enabled <- enabled

    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
    strategy$name
}

#' apply the rules in the strategy to arbitrary market data 
#' 
#' In typical usage, this function will be called via \code{\link{applyStrategy}}.  
#' In this mode, this function will be called twice, once with \code{path.dep=FALSE} 
#' and then again in stepping over the time indexes of the mktdata object.
#' 
#' This function, because of its path dependent nature and the order of rule 
#' evaluation discussed in \code{\link{add.rule}}, will likely take up most of the 
#' execution time of a strategy backtest.
#' 
#' Individual rule functions may need to use <<- to place \code{hold} and \code{holdtill}
#' variables into play.  These would be most likely implemented by risk rules.  When
#' \code{hold==TRUE}, any open oders will still be processed (orders are \emph{NOT} 
#' canceled automatically, but no new orders may be entered.  \code{type='risk'}
#' rules will still function during a hold.  Note that hold must be set via a custom
#' rule.  We tend to set hold in an order or risk rule. 
#' 
#' \code{quantstrat} has a significant amount of logic devoted to handling 
#' path-dependent rule execution.  Most of that code/logic resides in this
#' function.  
#' 
#' This function, along with \code{\link{ruleOrderProc}}, \code{\link{addOrder}}, and 
#' \code{\link{applyStrategy}} will likely need to be replaced to connect to a live 
#' market infrastructure. 
#' 
#' 
#' @section Dimension Reduction for Performance:
#' In evaluation of path-dependent rules, the simplest method, 
#' and the one we used initially, is to check the rules on every observation 
#' in the time series of market data.  
#' There are cases where this will still be required, but we hope to limit them as much as possible.
#' Looping in \R is generally discouraged, and on high frequency data for 
#' strategy evaluation it can produce completely unacceptable results.
#' 
#' The solution we've employed is to utilize a state machine to evaluate the rules only 
#' when deemed necessary.
#' This approach makes use of what we know about the strategy and
#' the orders the strategy places (or may place) to reduce the dimensionality of the problem.
#' 
#' As discussed in \code{\link{add.rule}}, the first step in this dimension 
#' reduction is to look for places in the time series where signals may cause the strategy to 
#' enter or change orders.  This creates an index of timestamps that must be evaluated.
#' This index should be significantly shorter than the full number of observations.    
#' \code{quantstrat} will always run \code{applyRules} on each of these indices
#' where we've previously figured out that the strategy might want to do something.
#' 
#' The next step in dimension reduction works on the order book.  
#' If there are open orders, we need to figure out when they might get filled.  
#' For market orders, this is the next observation.  For limit orders, we can 
#' locate the index timestamps after the order is placed to see when the
#' order might cross.  We will add this index to the list of indices to be 
#' evaluated.  There is of course no guarantee that the order will still be 
#' open at that time, that trading will not be on \code{hold} because of a risk rule, 
#' or that something else hasn't interfered.  Adding the index to the list only tells
#' the loop inside \code{applyRules} that rules (including order processing rules) 
#' need to be checked at that index, to see if anything needs to happen.
#' 
#' For trailing orders, the picture is somewhat more complicated.  Trailing orders
#' \emph{may} move on each new observation, per the method described in 
#' \code{\link{addOrder}}. To speed up evaluation of when such an
#' order may cross, we need to combine the possible crossing logic for 
#' the limit orders, above, with some additional logic to handle the 
#' trailing orders. We begin by evaluating when the order price might 
#' be moved. We then examine the market data between the current index and 
#' the point at which the order may move. if there is a (possible) cross, 
#' we insert that index into the indices for examination.  If not, we insert 
#' the index of the next probable move.
#' 
#' It should be noted that this dimension reduction methodology does 'look ahead'
#' in the data.  This 'look ahead' is only done \emph{after} the order has been 
#' entered in the normal path-dependent process, and only to insert new indices for 
#' evaluation, and so should not introduce biases.
#'      
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param strategy an object of type 'strategy' to add the rule to
#' @param mktdata an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param indicators if indicator output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param signals if signal output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param parameters named list of parameters to be applied during evaluation of the strategy,default NULL, only needed if you need special names to avoid argument collision
#' @param ... any other passthru parameters
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details 
#' @param rule.order default NULL, use at your own risk to adjust order of rule evaluation
#' @param debug if TRUE, return output list
#' @param rule.subset ISO-8601 subset for period to execute rules over, default NULL
#' @seealso \code{\link{add.rule}} \code{\link{applyStrategy}} 
#' @export
applyRules <- function(portfolio, 
                        symbol, 
                        strategy, 
                        mktdata, 
                        indicators=NULL, 
                        signals=NULL, 
                        parameters=NULL,   
                        ..., 
                        path.dep=TRUE,
                        rule.order=NULL,
                        rule.subset=NULL,
                        debug=FALSE) {
  print(symbol)
  assign("symbol", symbol, envir = .GlobalEnv)
  source("C:/Users/KIRTY/Documents/BlitzkriegTrading/production.applyRules.2B.R")
  return(NULL)
} #end applyRules

# private function ruleProc, used by applyRules and applyStrategy.rebalancing
ruleProc <- function (ruletypelist,timestamp=NULL, path.dep, ruletype, ..., parameters=NULL){
    
    for (rule in ruletypelist){
        #TODO check to see if they've already been calculated
        if (!rule$path.dep==path.dep) next()

        if(is.function(rule$name)) {
            ruleFun <- rule$name
        } else {
            if(exists(rule$name, mode="function")) {
                ruleFun <- get(rule$name, mode="function")
            } else {
                rule.name <- paste("rule", rule$name, sep=".")
                if(exists(rule.name, mode="function")) {
                    ruleFun <- get(rule.name, mode="function")
                    rule$name <- rule.name
                } else {
                    message("Skipping rule ", rule$name,
                            " because there is no function by that name to call")
                }
            }
        }

        if(!isTRUE(rule$enabled)) next()
        
        # check to see if we should run in this timespan
        if(!is.null(rule$timespan)) {
            # Get row index of timestamp for faster subsetting
            if(hasArg(curIndex))
                curIndex <- eval(match.call(expand.dots=TRUE)$curIndex, parent.frame())
            else
                curIndex <- timestamp
            if(nrow(mktdata[curIndex][rule$timespan])==0)
                next()
        }
        
        # modify a few things
        rule$arguments$timestamp = timestamp
        rule$arguments$ruletype  = ruletype
        rule$arguments$label = rule$label

        # replace default function arguments with rule$arguments
        .formals <- formals(rule$name)
        .formals <- modify.args(.formals, rule$arguments, dots=TRUE)
        # now add arguments from parameters
        .formals <- modify.args(.formals, parameters, dots=TRUE)
        # now add dots
        .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
        # remove ... to avoid matching multiple args
        .formals$`...` <- NULL
        
        # any rule-specific prefer-parameters should override global prefer parameter
        if(!is.null(rule$arguments$prefer)) .formals$prefer = rule$arguments$prefer
        
        # evaluate rule in applyRules' environment
        tmp_val <- do.call(ruleFun, .formals, envir=parent.frame(1))
                
#            print(paste('tmp_val ==', tmp_val))
    } #end rules loop
} # end sub process function ruleProc

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2015
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, 
# Jeffrey Ryan, Joshua Ulrich, and Garrett See 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
