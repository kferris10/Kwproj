#' Difference in Densities
#'
#' Calculates the difference in pitch density when pitchers are responsible for
#' the men on based and when they are not responsible.
dens_diff <- function(npoints, data = pit_LvL) {
  # calculating densities when pitchers are responsible for men on base
  resp_dens <- data %>%
    filter(resp_pit == "resp") %$%
    MASS::kde2d(px, pz, n = npoints)

  # calculating densities when pitchers are not responsible for men on base
  not_dens <- data %>%
    filter(resp_pit == "not") %$%
    MASS::kde2d(px, pz, n = npoints)

  # calculating observed difference in densities
  not_dens$z - resp_dens$z
}

#' Permuted Differences
#'
#' Permutes the data B times and calculates the difference in densities for each
#' of the B permutations.
perm_diffs <- function(B, npoints, data = pit_LvL) {
  diffs <- array(NA, dim = c(npoints, npoints, B))

  for(i in 1:B) {
    # permuted labels
    perm_vec <- sample(pit_LvL$resp_pit)

    # permuted densities
    rperm_dens <- data %>%
      filter(perm_vec == "resp") %$%
      kde2d(px, pz, n = npoints)
    nperm_dens <- data %>%
      filter(perm_vec == "not") %$%
      kde2d(px, pz, n = npoints)

    # differences
    diffs[, , i] <- nperm_dens$z - rperm_dens$z

  }

  diffs
}

#' Calulate Permuted P-values
#'
#' For each density point on the grid, finds the proportion of permuted
#' densities which are more extreme that the observed density.
calc_pvals <- function(obs_diff, perms) {
  # number of grid points
  npoints <- nrow(obs_diff)

  # obtaining p-values
  pvals <- matrix(NA, nrow = npoints, ncol = npoints)
  for(i in 1:npoints) {
    for(j in 1:npoints) {
      dat <- data.frame(perm_vals = perms[i, j, ])
      obs_val <- obs_diff[i, j]
      pvals[i, j] <- mean(abs(dat$perm_vals) > abs(obs_val))
    }
  }
  pvals
}

#' Obtain p-values from Permutations
#'
#' Wrapper which uses \code{\link{dens_diff}}, \code{\link{perm_diffs}}, and
#' \code{\link{calc_pvals}} to obtain p-values at each grid point.
perm_pvals <- function(B, npoints, data = pit_LvL, obs_diff = NULL) {
  # observed difference
  if(!is.null(obs_diff)) obs_diff <- dens_diff(npoints, data)

  # permutations
  perm_dens <- perm_diffs(B, npoints, data)

  # obtaining p-values
  calc_pvals(obs_diff, perm_dens)
}

#' Turns matrix of p-values into a \code{\link{tbl}} into long format.
#'
#' A nice way to turn a matrix of p-values into long format.
pvals_to_long <- function(dat) {
  # turning into long data frame and returning
  dat %>%
    data.frame() %>%
    tbl_df() %>%
    gather(location, pval) %>%
    mutate(col = rep(1:npoints, each = npoints),
           row = rep(1:npoints, npoints)) %>%
    select(row, col, pval)
}
