#' R Hyperparameters optimization using Bender Optimizer (bender.dreem.com)
#'
#' A simple http based interface to Bender API.

#' @name bender
#' @aliases bender
#' @docType package
NULL

#' @import R6
#' @import httr
#' @import jsonlite

base_url = 'https://bender-api.dreem.com/'

handle_request_errors <- function(response) {
  if(http_error(response)) {
    stop(content(response))
  }
}

check_user_connected <- function(mail) {
  if(is.null(mail)) {
    stop("Oops! Seems like you are not connected to the Bender API, please re-instantiate the Bender object with correct credentials.")
  }
}

#' Bender Client
#'
#' The Bender Class. Provides an interface to the Bender API. The goal with this client is to feed Bender with various trials.
#' Doing so, he will be more and more precise in the suggestions of hyperparameters set he makes.
#'
#' \strong{/!\\ For fully detailed documentation on slots and methods, please visit} \url{https://bender-optimizer.readthedocs.io/en/latest/index.html}
#'
#' @section Usage:
#' \preformatted{
#' bender = Bender$new("MyBenderMail", "MySecretPassword")
#'
#' bender$set_algo(id="88155b59-aca1-4cb1-898c-25d942c02859")
#'
#' suggested_hyperparameters <- bender$suggest(metric="accuracy")
#' results <- my_optimization_problem(my_data, suggested_hyperparameters)
#' bender$create_trial(list(accuracy=results.accuracy), suggested_hyperparameters)
#'
#' }
#'
#' @section Slots:
#' \code{mail} Mail of connected user.
#'
#' \code{token} A unique JWT associated to your current session.
#'
#' \code{experiment} The experiment object.
#'
#' \code{algo} The algo object.
#'
#' @section Methods:
#' \code{$new(mail, password)} Initialize a Bender object and establish connection to the API.
#'
#' \code{$list_experiments()} Return a list of the connected user’s experiments.
#'
#' \code{$set_experiment(name=NULL, id=NULL)} Setup current experiment for the connected user.
#'
#' \code{$create_experiment(name, metrics, description=NULL, dataset=NULL, dataset_parameters=NULL)} Create new experiment and setup current experiment for the connected user. If experiment name already exists, the already existing one is set by default as current experiment.
#'
#' \code{$delete_experiment(id)} Delete targeted experiment of the connected user.
#'
#' \code{$list_algos()} Return a list of the connected user’s algos.
#'
#' \code{$set_algo(id=NULL, name=NULL)} Setup current algo for the connected user.
#'
#' \code{$set_algo(name, hyperparameters, description=NULL)} Create new algo and setup current algo for the connected user. If algo name already exists, the already existing one is set by default as current algo.
#'
#' \code{$delete_algo(id)} Delete targeted algo of the connected user.
#'
#' \code{$list_trials()} List all trials of the current algo.
#'
#' \code{$create_trial(results, hyperparameters, weight=1, comment=NULL)} Create new trial for the current algo.
#'
#' \code{$delete_trial(id)} Delete targeted trial from current algo.
#'
#' \code{$suggest(metric=NULL, optimizer="parzen_estimator")} Ask bender a suggestion on a hyperparameters set to use
#'
#' \code{$delete_trial(id)} Delete targeted trial from current algo.
#'
#' @name Bender
NULL

#' @export
Bender <- R6Class("Bender", cloneable = FALSE, list(
  mail = NULL,
  token = NULL,
  experiment = NULL,
  algo = NULL,
  initialize = function(mail, password) {
    if(!hasArg(mail) || !hasArg(password) || !is.character(mail) || !is.character(password)) {
      stop("Please provide a mail string and a password string.")
    }
    response = POST(paste(base_url, 'login/', sep=''), body = list(mail=mail, password=password), encode = "json")
    handle_request_errors(response)
    self$token <- paste('JWT', content(response)$token)
    self$mail <- content(response)$user$mail
    message('Bender successfully initialized !')
  },
  list_experiments = function() {
    check_user_connected(self$mail)
    response = GET(paste(base_url, 'api/experiments/?owner=', self$mail, sep=''), add_headers(Authorization = self$token))
    handle_request_errors(response)
    experiments = c()
    for (exp in content(response)[['results']]) {
      experiments <- c(experiments, list(list(id=exp[['id']], name=exp[['name']])))
    }
    return(experiments)
  },
  set_experiment = function(name=NULL, id=NULL) {
    if (!is.null(id)) {
      response = GET(paste(base_url, 'api/experiments/', id, '/', sep=''), add_headers(Authorization = self$token))
      handle_request_errors(response)
      self$experiment <- content(response)
      message('Experiment successfully set !')
    } else if (!is.null(name)) {
      response = GET(paste(base_url, 'api/experiments/?owner=', self$mail, '&name=', name, sep=''), add_headers(Authorization = self$token))
      handle_request_errors(response)
      self$experiment <- content(response)[['results']][[1]]
      message('Experiment successfully set !')
    } else {
      stop("Please provide a name or an id")
    }
  },
  create_experiment = function(name, metrics, description=NULL, dataset=NULL, dataset_parameters=NULL) {
    response = GET(paste(base_url, 'api/experiments/?owner=', self$mail, '&name=', name, sep=''), add_headers(Authorization = self$token))
    if(http_status(response)$category == "Success"  && content(response)[['count']] == 1) {
      self$experiment <<- content(response)[['results']][[1]]
      stop("Experiment already exist with that name and is now set as the current experiment.")
    } else {
      if (typeof(metrics) != "list") {
        stop("Need to give a list of metrics.")
      }
      response = POST(paste(base_url, 'api/experiments/', sep=''), handle=handle(''), body = list(name=name, metrics=metrics, description=description, dataset=dataset, dataset_parameters=dataset_parameters), add_headers(Authorization = self$token), encode = "json")
      handle_request_errors(response)
      self$experiment <- content(response)[['id']]
      message('Experiment successfully created !')
    }
  },
  delete_experiment = function(id) {
    response = DELETE(paste(base_url, 'api/experiments/', id, '/', sep=''), handle=handle(''), add_headers(Authorization = self$token))
    handle_request_errors(response)
    message('Experiment successfully deleted !')
  },
  list_algos = function() {
    if(is.null(self$experiment)) {
      stop("Need to set an experiment first.")
    }
    response = GET(paste(base_url, 'api/algos/?experiment=', self$experiment$id, sep=''), add_headers(Authorization = self$token))
    handle_request_errors(response)
    algos = c()
    for (algo in content(response)[['results']]) {
      algos <- c(algos, list(list(id=algo[['id']], name=algo[['name']])))
    }
    return(algos)
  },
  set_algo = function(id=NULL, name=NULL) {
    if (!is.null(id)) {
      response = GET(paste(base_url, 'api/algos/', id, '/', sep=''), add_headers(Authorization = self$token))
      handle_request_errors(response)
      self$algo <- content(response)
      if (is.null(self$experiment) || self$algo$experiment != self$experiment$id) {
        self$set_experiment(id=self$algo$experiment)
      }
      message('Algo successfully set !')
    } else if (!is.null(name)) {
      if(is.null(self$experiment)) {
        stop("Set the algo by id or set an experiment first if you want to set it by name.")
      }
      response = GET(paste(base_url, 'api/algos/?experiment=', self$experiment$id, '&name=', name, sep=''), add_headers(Authorization = self$token))
      handle_request_errors(response)
      self$algo <- content(response)[['results']][[1]]
      if (is.null(self$experiment) || self$algo$experiment != self$experiment$id) {
        self$set_experiment(id=self$algo$experiment)
      }
      message('Algo successfully set !')
    } else {
      stop("Please provide a name or an id")
    }
  },
  create_algo = function(name, hyperparameters, description=NULL) {
    if(is.null(self$experiment)) {
      stop("Need to set an experiment first.")
    }
    response = GET(paste(base_url, 'api/algos/?experiment=', self$experiment$id, '&name=', name, sep=''), add_headers(Authorization = self$token))
    if(http_status(response)$category == "Success" && content(response)[['count']] == 1) {
      self$algo <<- content(response)[['results']][[1]]
      self$set_experiment(id=self$algo$experiment)
      stop("Algo already exist with that name and is now set as the current algo.")
    } else {
      if (typeof(hyperparameters) != "list") {
        stop("Need to give a list of hyperparameters.")
      }
      response = POST(paste(base_url, 'api/algos/', sep=''), handle=handle(''), body = list(name=name, parameters=hyperparameters, description=description, experiment=self$experiment$id), add_headers(Authorization = self$token), encode = "json")
      handle_request_errors(response)
      self$algo <- content(response)[['id']]
      message('Algo successfully created !')
    }
  },
  delete_algo = function(id) {
    response = DELETE(paste(base_url, 'api/algos/', id, '/', sep=''), handle=handle(''), add_headers(Authorization = self$token))
    handle_request_errors(response)
    message('Algo successfully deleted !')
  },
  list_trials = function() {
    if(is.null(self$algo)) {
      stop("Need to set an algo first.")
    }
    response = GET(paste(base_url, 'api/trials/?algo=', self$algo$id, sep=''), add_headers(Authorization = self$token))
    handle_request_errors(response)
    return(content(response)[['results']])
  },
  create_trial = function(results, hyperparameters, weight=1, comment=NULL) {
    if(is.null(self$algo)) {
      stop("Need to set an algo first.")
    }
    if (typeof(results) != "list") {
      stop("Need to give a list of results")
    }
    if (typeof(hyperparameters) != "list") {
      stop("Need to give a list of hyperparameters.")
    }
    response = POST(paste(base_url, 'api/trials/', sep=''), handle=handle(''), body = list(weight=weight, comment=comment, parameters=hyperparameters, results=results, algo=self$algo$id), add_headers(Authorization = self$token), encode = "json")
    handle_request_errors(response)
    message('Trial successfully added !')
  },
  delete_trial = function(id) {
    response = DELETE(paste(base_url, 'api/trials/', id, '/', sep=''), handle=handle(''), add_headers(Authorization = self$token))
    handle_request_errors(response)
    message('Trial successfully deleted !')
  },
  suggest = function(metric=NULL, optimizer="parzen_estimator") {
    if(is.null(self$experiment)) {
      stop("Need to set an experiment first.")
    }
    if(is.null(self$algo)) {
      stop("Need to set an algo first.")
    }
    if(is.null(metric)) {
      stop("Please indicate a metric to optimize!")
    }
    if(!b$algo$is_search_space_defined) {
      stop("Must define a search space properly.")
    }
    response = POST(paste(base_url, '/api/algos/', self$algo$id, '/suggest/', sep=''), handle=handle(''), body = list(metric=metric, optimizer=optimizer), add_headers(Authorization = self$token))
    handle_request_errors(response)
    return(content(response))
  })
)
