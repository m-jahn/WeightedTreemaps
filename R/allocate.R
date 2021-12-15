#' @importFrom stats rnorm
#' @importFrom dplyr %>%
#' @importFrom sf st_area
#' 
cellError <- function(a, target) {
  normA <- a / sum(a)
  diff <- abs(normA - target)
  max(diff)
}

breaking <- function(
  a, target,
  max = TRUE,
  debug = FALSE,
  error_tol,
  prevError) {
  
  if (max) {
    
    # Stop when largest individual cell error is less than 1%
    # (the default)
    err <- cellError(a, target)
    if (is.na(err) | is.nan(err)) err <- 1
    if (debug)
      message(paste("Difference: ", err,
                " (", abs(err - prevError), ")",
                "\n", sep = ""))
    stopping <- {err < error_tol}
    prevError <- err

  } else {
    
    normA <- a / sum(a)
    diff <- abs(normA - target)
    # Stop when *change* in *total* cell error is tiny
    # (i.e., we are not improving the solution)
    if (debug)
      message(paste(
        "Difference: ",
        round(sum(diff), 2),
        " (",
        round(abs(sum(diff) - prevError), 3),
        ")",
        "\n",
        sep = ""
      ))
    stopping <- abs(sum(diff) - prevError) < 0.001
    prevError <- sum(diff)
    
  }
  list(
    stopping = stopping,
    prevError = prevError
  )
}


# Instead of adjusting by a multiple of current weight
# adjust by multiple of average absolute weights
# This avoids problem of getting stuck at a tiny weight
# (and stabilizes the algorithm generally)
# difference to original implementation: adjustment of maximal 
# step change of weights to prevent crashing of algorithm
adjustWeights <- function(w, a, target) {
  normA <- a / sum(a)
  # OPTION: avoid extreme scaling values -> squareroot function
  # to buffer strong difference between computed area and target
  # and to buffer the global weight increase
  # these increase stability but also computation time:
  #scaling <- ifelse(scaling < -1, scaling/sqrt(abs(scaling)), scaling)
  #w + sqrt(mean(abs(w))) * scaling
  scaling <- ((target - normA) / target)
  w + mean(abs(w)) * scaling
}


shiftSites <- function(s, k) {
  newSites <- mapply(function(poly, x, y) {
    # Handle empty polygons
    if (length(poly)) {
      poly_centroid(poly[[1]][, 1], poly[[1]][, 2])
    } else {
      c(x = x, y = y)
    }
  },
  k, as.list(s$x), as.list(s$y),
  SIMPLIFY = FALSE)
  list(x = sapply(newSites, "[", "x"),
       y = sapply(newSites, "[", "y"))
}


# Variation from published algorithm
# Allow factor adjustment per pair of sites
shiftWeights <- function(s, w) {
  n <- length(s$x)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        # Deviation from published algorithm here
        # to use abs(w) so that ensure non-overlapping
        # circles even when weights are negative
        f = sqrt((s$x[i] - s$x[j]) ^ 2 +
                   (s$y[i] - s$y[j]) ^ 2) / (abs(w[i]) + abs(w[j]))
        if (f > 0 && f < 1) {
          w[i] <- w[i] * f
          w[j] <- w[j] * f
        }
      }
    }
  }
  w
}

# The algorithm can fail to converge sometimes so
# just give up after 'maxIteration's
allocate <- function(
  names, s, w, outer, target,
  maxIteration, 
  error_tol,
  debug = FALSE, 
  debugCell = FALSE)
{
  count <- 1
  prevError <- 1
  
  repeat {
    
    # if all weights are identical the CGAL algorithm often fails
    # in this case we introduce a bit of random variation
    if (length(unique(w)) == 1) {
      w <- w * rnorm(length(w), mean = 1, sd = 0.01)
    }
    
    # call to awv function, the additively weighted voronoi tesselation,
    # wrapped within a trycatch statement to catch errors and start over
    k <- tryCatch(awv(s, w, outer, debug, debugCell),
      error = function(e) { print(e); NULL}
    )
    if (is.null(k)) {
      return(NULL)
    }
    areas <- lapply(k, sf::st_area)
    
    # if debug=TRUE, every iteration is drawn to the viewport
    # this can be very time and resource consuming and should be used 
    # with care. The result resembles the final treemap but is an overlay of
    # many iterations
    if (debug) {
      drawRegions(
        list(names = names,
             k = k, s = s,
             w = w, a = areas,
             t = target
        ),
        debug, label = TRUE, label.col = grey(0.5),
        lwd = 2, col = grey(0.5),
        fill = grey(1, alpha=0.33)
      )
      
      info <-
        rbind(
          area = round(unlist(areas) / sum(unlist(areas)), 4),
          target = round(target, 4),
          weight = round(w, 1),
          areaAbs = round(unlist(areas))
        )
      colnames(info) <- names
      print(info)
    }
    
    stop_cond <- breaking(
      unlist(areas), 
      target, 
      debug = debug,
      error_tol = error_tol,
      prevError = prevError)
    
    # if stop condition is fulfilled, return result in form of
    # list of polygons and metadata
    if (count == maxIteration || stop_cond$stopping) {
      
      res <- lapply(1:length(names), function(i) {
        list(
          name = names[i], poly = k[[i]],
          site = c(s$x[[i]], s$y[[i]]), 
          weight = w[i], area = unlist(areas)[i], 
          target = target[i],
          count = count)
      }) %>% setNames(names)
      return(res)
    
    } else {
      
      w <- adjustWeights(w, unlist(areas), target)
      s <- shiftSites(s, k)
      w <- shiftWeights(s, w)
    }
    count <- count + 1
    prevError = stop_cond$prevError
  }
}
