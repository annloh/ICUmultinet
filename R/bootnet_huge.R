### HUGE ESTIMATOR ###
bootnet_huge <- function(
  data, # Dataset used
  tuning = 0.5,
  missing = c("listwise","stop"),
  verbose = TRUE,
  npn = TRUE, # Compute nonparanormal?
  criterion = c("ebic","ric","stars"),
  principalDirection = FALSE,
  lambda.min.ratio = 0.01,
  nlambda = 100,
  unlock = FALSE,
  transform = c("none","rank","quantile"),
  ...){

  transform <- match.arg(transform)
  if (transform == "rank"){
    data <- rank_transformation(data)
  } else if (transform == "quantile"){
    data <- quantile_transformation(data)
  }

  if (!unlock){
    stop("You are using an internal estimator function without using 'estimateNetwork'. This function is only intended to be used from within 'estimateNetwork' and will not run now. To force manual use of this function (not recommended), use unlock = TRUE.")
  }


  # Check arguments:
  missing <- match.arg(missing)
  criterion <- match.arg(criterion)
  # method <- match.arg(method)

  # Message:
  if (verbose){
    msg <- "Estimating Network. Using package::function:"
    msg <- paste0(msg,"\n  - huge::huge for network computation")
    msg <- paste0(msg,"\n  - huge::huge.npn for nonparanormal transformation")
    # msg <- paste0(msg,"\n\nPlease reference accordingly\n")
    message(msg)
  }

  # First test if data is a data frame:
  if (!(is.data.frame(data) || is.matrix(data))){
    stop("'data' argument must be a data frame")
  }

  # If matrix coerce to data frame:
  if (is.matrix(data)){
    data <- as.data.frame(data)
  }

  # Obtain info from data:
  N <- ncol(data)
  Np <- nrow(data)


  # Check missing:
  if (missing == "stop"){
    if (any(is.na(data))){
      stop("Missing data detected and missing = 'stop'")
    }
  } else {
    # listwise:
    data <- na.omit(data)
  }

  # Nonparanormal:
  if (npn){
    data <- huge::huge.npn(na.omit(as.matrix(data)),verbose = verbose)
  }

  # Principal direction:
  if (principalDirection){
    data <- principalDirection_noCor(data)
  }

  # Estimate network:
  Results <- huge::huge.select(huge::huge(as.matrix(data),method = "glasso",
                                          verbose=verbose,
                                          lambda.min.ratio=lambda.min.ratio,
                                          nlambda=nlambda),
                               criterion = criterion,
                               verbose = verbose,
                               ebic.gamma = tuning)

  # Return:
  return(list(
    graph=as.matrix(qgraph::wi2net(as.matrix(Results$opt.icov))),
    results=Results))
}

assignInNamespace(x = "bootnet_huge", value = bootnet_huge, ns = "bootnet")
