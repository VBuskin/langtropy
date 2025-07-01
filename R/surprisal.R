#' Calculate Surprisal (Self-Information)
#'
#' Computes the surprisal (also known as self-information) of an event given
#' its probability. Surprisal quantifies how "surprising" an event is - rare
#' events have high surprisal, while common events have low surprisal.
#'
#' @param p A numeric vector of probabilities. Values must be between 0 and 1
#'   (exclusive of 0, inclusive of 1).
#' @param base The logarithmic base to use. Default is 2 (bits). Use \code{exp(1)}
#'   for nats, or 10 for dits/bans.
#' @param na.rm Logical. Should missing values be removed? Default is FALSE.
#'
#' @return A numeric vector of surprisal values in the specified units.
#'   Returns \code{Inf} for probability 0, and 0 for probability 1.
#'
#' @details
#' Surprisal is calculated as: \eqn{I(x) = -\log_b(P(x))}
#' 
#' Where:
#' \itemize{
#'   \item \code{I(x)} is the surprisal of event x
#'   \item \code{P(x)} is the probability of event x
#'   \item \code{b} is the logarithmic base
#' }
#'
#' Common bases and their units:
#' \itemize{
#'   \item Base 2: bits (most common in information theory)
#'   \item Base e: nats (natural units)
#'   \item Base 10: dits or bans
#' }
#'
#' @examples
#' # Surprisal of a fair coin flip (0.5 probability)
#' surprisal(0.5)  # 1 bit
#' 
#' # Surprisal of various probabilities
#' probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
#' surprisal(probs)
#' 
#' # Using different bases
#' surprisal(0.5, base = exp(1))  # in nats
#' surprisal(0.5, base = 10)      # in dits
#' 
#' # With a linguistic example: word frequencies
#' word_probs <- c(0.1, 0.05, 0.001, 0.2)  # probabilities of different words
#' word_surprisals <- surprisal(word_probs)
#' names(word_surprisals) <- c("the", "cat", "serendipity", "and")
#' word_surprisals
#'
#' @export
#' @family information theory functions
surprisal <- function(p, base = 2, na.rm = FALSE) {
  # Input validation
  if (!is.numeric(p)) {
    stop("Argument 'p' must be numeric.")
  }
  
  if (!is.numeric(base) || length(base) != 1 || base <= 0 || base == 1) {
    stop("Argument 'base' must be a single positive number not equal to 1.")
  }
  
  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("Argument 'na.rm' must be a single logical value.")
  }
  
  # Handle missing values
  if (na.rm) {
    p <- p[!is.na(p)]
  } else if (any(is.na(p))) {
    warning("Missing values detected. Use na.rm = TRUE to remove them.")
  }
  
  # Check probability bounds
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop("All probabilities must be between 0 and 1 (inclusive).")
  }
  
  # Calculate surprisal: -log_base(p)
  # Note: log(0) returns -Inf, and -(-Inf) = Inf, which is correct for surprisal
  result <- -log(p, base = base)
  
  return(result)
}