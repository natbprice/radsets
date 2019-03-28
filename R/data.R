#' Genre classifications of 27,000 movies
#'
#' A dataset containing the genres and other attributes of 27,000
#' movies.
#'
#' @format A data frame with 27278 rows and 27 variables:
#' \describe{
#'   \item{movieId}{MovieLens id (https://movielens.org/movies/\code{movieId})}
#'   \item{title}{movie title}
#'   \item{year}{year of release}
#'   \item{imdbId}{IMDB id (http://www.imdb.com/title/tt\code{imdbId})}
#'   \item{tmdbId}{The Movie DB id (https://www.themoviedb.org/movie/\code{tmdbId})}
#'   \item{avgRating}{average movie rating}
#'   \item{nRating}{number of ratings}
#'   \item{genres}{movie genres as string separated by "|"}
#'   \item{Action:Western}{binary columns indicating set membership}
#' }
#' @source \url{https://grouplens.org/datasets/movielens/20m/}
"movieSets"

#' Point-of-sale transaction data from grocery outlet
#'
#' This dataset was obtained from the arules package
#' (\url{https://cran.r-project.org/package=arules}) and converted
#' to a data frame. Please see \code{\link[arules]{Groceries}} in the arules
#' package and cite accordingly.
#'
#' @format A data frame with 9835 rows and 170 variables:
#' \describe{
#'   \item{frankfurter:bags}{logical columns indicating set membership}
#'   \item{id}{a transaction id based on row number}
#' }
#' @source \url{https://cran.r-project.org/package=arules}
#'
#' @references
#' Michael Hahsler, Kurt Hornik, and Thomas Reutterer (2006) Implications of
#'  probabilistic data modeling for mining association rules.
#'  In M. Spiliopoulou, R. Kruse, C. Borgelt, A. Nuernberger, and W. Gaul,
#'   editors, From Data and Information Analysis to Knowledge Engineering,
#'   Studies in Classification, Data Analysis, and Knowledge Organization,
#'   pages 598–605. Springer-Verlag.
#'
"groceries"
