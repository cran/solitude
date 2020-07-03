#' @name isolationForest
#' @title Fit an Isolation Forest
#' @description 'solitude' class implements the isolation forest method
#'   introduced by paper Isolation based Anomaly Detection (Liu, Ting and Zhou
#'   <doi:10.1145/2133360.2133363>). The extremely randomized trees (extratrees)
#'   required to build the isolation forest is grown using
#'   \code{\link[ranger]{ranger}} function from \pkg{ranger} package.
#' @section Design: \code{$new()} initiates a new 'solitude' object. The
#'   possible arguments are:
#'
#'   \itemize{
#'
#'   \item \code{num_trees}: (positive integer, default = 100) Number of trees
#'   to be built in the forest
#'
#'   \item \code{sample_fraction}: ((0, 1], default = 1) Fraction of the dataset
#'   to be sampled or bootstrapped per tree. See 'sample.fraction' argument in
#'   \code{\link[ranger]{ranger}}
#'
#'   \item \code{replace}: (boolean, default = FALSE) Whether the sample of
#'   observations for each tree should be chosen with replacement. See 'replace'
#'   argument in \code{\link[ranger]{ranger}}
#'
#'   \item \code{seed}: (positive integer, default = 101) Random seed for the
#'   forest
#'
#'   \item \code{nproc}: (a positive integer, default: one less than maximum
#'   number of scores available) Number of parallel threads to be used by ranger
#'
#'   \item \code{respect_unordered_factors}: (string, default: "partition") See
#'   'respect.unordered.factors' argument in \code{\link[ranger]{ranger}}
#'
#'   }
#'
#'   \code{$fit()} fits a isolation forest for the given dataframe, computes
#'   depths of terminal nodes of each tree and stores the anomaly scores and
#'   average depth values in \code{$scores} object as a data.table
#'
#'   \code{$predict()} returns anomaly scores for a new data as a data.table
#'
#' @section Details:
#'
#'   \itemize{
#'
#'   \item Parallelization: \code{\link[ranger]{ranger}} is parallelized and by
#'   default uses all cores but one. The process of obtaining depths of terminal
#'   nodes (which is executed with \code{$fit()} is called) may be parallelized
#'   separately by setting up a \pkg{future} backend.
#'
#'   }
#'
#' @examples
#' data("humus", package = "mvoutlier")
#' columns_required = setdiff(colnames(humus)
#'                            , c("Cond", "ID", "XCOO", "YCOO", "LOI")
#'                            )
#' humus2 = humus[ , columns_required]
#' set.seed(1)
#' index = sample(ceiling(nrow(humus2) * 0.5))
#' isf = isolationForest$new()  # initiate
#' isf$fit(humus2[index, ])     # fit on 80% data
#' isf$scores                   # obtain anomaly scores
#'
#' # scores closer to 1 might indicate outliers
#' plot(density(isf$scores$anomaly_score))
#'
#' isf$predict(humus2[-index, ]) # scores for new data
#' @export

isolationForest = R6::R6Class(
  "solitude"
  ,
  # Implementation:
  # 'solitude' class implements the isolation forest method defined by
  # Liu, Fei Tony, Ting, Kai Ming and Zhou, Zhi-Hua.
  # "Isolation-based anomaly detection." ACM Transactions on Knowledge Discovery   # from Data (T KDD). <doi:10.1145/2133360.2133363>
  #
  # Plan:
  # 1. Build 'extratrees' forest using ranger.
  # 2. Obtain terminal node depths by parsing each tree.
  # 3. Compute anomaly scores.
  #
  # Design:
  # 1. 'initialize' method sets the params required for forest growth.
  # 2. Calling 'fit' on some dataset does these things:
  #    - Grow 'extratrees' forest.
  #    - Compute and store the depth of each terminal node in every tree.
  #    - Compute depth or pathlength for each observation.
  #    - Compute anomaly score for each observation and store it in 'scores'.
  # 3. 'predict' method computes anomaly scores on new data.
  #
  public = list(

    num_trees                   = NULL
    , replace                   = NULL
    , seed                      = NULL
    , nproc                     = NULL
    , respect_unordered_factors = NULL
    , sample_fraction           = NULL
    , scores                    = NULL
    , status                    = "not_initialized"
    ,
    # intialize arguments required for fitting extratrees via ranger
    initialize = function(
      num_trees                   = 100
      , replace                   = FALSE
      , sample_fraction           = 1
      , respect_unordered_factors = "partition"
      , seed                      = 101
      , nproc                     = parallel::detectCores() -1
      ){

      stopifnot(is_integerish(num_trees) && length(num_trees) == 1)
      stopifnot(0 < num_trees)
      stopifnot(is.logical(replace) && length(replace) == 1)
      stopifnot(is_integerish(seed) && length(seed) == 1)
      stopifnot(is_integerish(nproc) && length(nproc == 1) && nproc >= 1)
      stopifnot(is.character(respect_unordered_factors) &&
                  length(respect_unordered_factors) == 1)
      stopifnot(sample_fraction <= 1 && sample_fraction > 0)

      self$num_trees                 = num_trees
      self$replace                   = replace
      self$seed                      = seed
      self$nproc                     = nproc
      self$respect_unordered_factors = respect_unordered_factors
      self$sample_fraction           = sample_fraction

      self$status = "not_trained"
    }
    ,
    fit = function(dataset){

      # create new fit
      if(self$status == "trained"){
        self$status = "not_trained"
        warning("Retraining ... all previous training results will be lost")
      }

      # create a new 'y' column with jumbled 1:n
      columnNames  = colnames(dataset)
      nr           = nrow(dataset)
      nc           = ncol(dataset)
      responseName = columnNames[[1]]
      while (deparse(substitute(responseName)) %in% columnNames) {
        responseName = sample(c(letters, LETTERS), 20, replace = TRUE)
      }
      set.seed(self$seed + 1)
      dataset[[deparse(substitute(responseName))]] = sample(nrow(dataset))

      # build a extratrees forest
      message("Building Isolation Forest ... ", appendLF = FALSE)
      private$forest = ranger::ranger(
        dependent.variable.name     = deparse(substitute(responseName))
        , data                      = dataset
        , mtry                      = nc
        , min.node.size             = 1L
        , splitrule                 = "extratrees"
        , num.random.splits         = 1L
        , num.trees                 = self$num_trees
        , replace                   = self$replace
        , sample.fraction           = self$sample_fraction
        , respect.unordered.factors = self$respect_unordered_factors
        , num.threads               = self$nproc
        , oob.error                 = FALSE
        , seed                      = self$seed
        )
      message("done")

      # compute terminal nodes depth
      message("Computing depth of terminal nodes ... ", appendLF = FALSE)
      private$terminal_nodes_depth = terminalNodesDepth(private$forest)
      message("done")

      # set phi -- sample size used for tree building
      private$phi = floor(self$sample_fraction * nr)

      # predict anomaly scores for data used for fitting
      self$scores = self$predict(dataset)

      # update train status
      self$status = "trained"
    }
    ,
    predict = function(data){

      tnm = stats::predict(private$forest
                           , data
                           , type        = "terminalNodes"
                           , num.threads = self$nproc
                           )[["predictions"]]

      tnm = data.table::as.data.table(tnm)
      data.table::setnames(tnm, colnames(tnm), as.character(1:ncol(tnm)))
      tnm[, id := .I]
      tnm = data.table::melt(tnm
                             , id.vars         = "id"
                             , variable.name   = "id_tree"
                             , value           = "id_node"
                             , variable.factor = FALSE
                             )
      id_tree = NULL
      id_node = NULL

      tnm[, id_tree := as.integer(id_tree)]
      tnm[, id_node := as.integer(id_node)]

      obs_depth = merge(tnm
                        , private$terminal_nodes_depth
                        , by = c("id_tree", "id_node")
                        )

      # create extra path length when terminal nodes are not singletons
      path_length_extend = tnm[ , .N, c("id_tree", "id_node")]
      path_length_extend[
          , extend := vapply(N, private$pathLengthNormalizer, numeric(1))
          ]

      # extend length depending on terminal node
      obs_depth = merge(obs_depth
                        , path_length_extend
                        , by = c("id_tree", "id_node")
                        )
      obs_depth[ , depth := depth + extend]
      obs_depth[ , c("N", "extend") := NULL]

      average_depth = NULL
      depth         = NULL
      id            = NULL
      anomaly_score = NULL

      scores = obs_depth[ , .(average_depth = mean(depth)
                              , median_depth = median(depth)
                              )
                          , by = id][order(id)]
      scores[, anomaly_score :=
               private$computeAnomaly(average_depth, private$phi)]
      data.table::setorder(scores, -anomaly_score)

      return(scores[])
    }
  )
  ,
  private = list(

    forest                       = NULL
    , terminal_nodes_depth       = NULL
    , phi                        = NULL
    ,
    pathLengthNormalizer = function(phi){

      res = 0

      if(phi == 2){
        res = 1
      }
      if(phi > 2){
        res = (2 * private$harmonic(phi - 1)) - (2 * (phi - 1)/phi)
      }

      return(res)

    }
    ,
    computeAnomaly = function(depth, data_size){

      den = private$pathLengthNormalizer(data_size)
      return(2^( -( depth / den ) ))
    }
    ,
    harmonic = function(n){
      ifelse(n > 11, log(n) + 0.577216, sum(1/seq(1,n)))
    }
  )
)
