#' Calculate on-level factors
#'
#' Calculate on-level factors
#' @param in.rc The rate change history.  in.rc is a data frame with three columns.  Column 1 is named "effdt" and is of type Date.  Column 2 is named "rc" and is of type numeric (note that rc is centered around zero, and not a rate level factor centered around unity).  Column 3 is named "type" and is of type char.  Possible values are either "R" for rate change or "L" for law change.
#' @param in.yr The historical periods for which the on-level factors will be calculated.  in.yr is a data frame with two columns.  Column 1 is named "effdt" and if of type Date.  effdt is the inclusive effective date.  Column 2 is named "expdt" and is of type Date.  expdt is the exclusive expiration date.
#' @param in.prem.type "E" = earned premium, "W" = written premium
#' @param in.year.type "C" = calendar year, "P" = policy year
#' @param in.policy.length Integer representing the length of a policy in months.
#' @return A list of five on-level factor components.
#' @export
olf <- function(in.rc, in.yr, in.prem.type, in.year.type, in.policy.length){
  # https://google.github.io/styleguide/Rguide.xml
  # Error handling for in.prem.type
  if (missing(in.prem.type)){
    in.prem.type <- "E"
  } else{
    if (length(in.prem.type) > 1){
      warning("in.prem.type should be of length 1, using first element only")
    }
    if (!(toupper(in.prem.type[1]) %in% c("E","W"))){
      stop("in.prem.type must be 'E' for earned or 'W' for written")
    } else{
      in.prem.type <- toupper(in.prem.type[1])
    }
  }
  # Error handling for in.year.type
  if (missing(in.year.type)){
    in.year.type <- "C"
  } else{
    if (length(in.year.type) > 1){
      warning("in.year.type should be of length 1, using first element only")
    }
    if (!(toupper(in.year.type[1]) %in% c("C","P"))){
      stop("in.year.type must be 'C' for calendar or 'P' for policy")
    } else{
      in.year.type <- toupper(in.year.type[1])
    }
  }
  # Error handling for in.policy.length
  if (missing(in.policy.length)){
    in.policy.length <- 12
  } else{
    if (length(in.policy.length) > 1){
      warning("in.policy.length should be of length 1, using first element only")
    }
    if (in.policy.length[1] <= 0){
      stop("in.policy.length must be positive")
    } else{
      in.policy.length <- in.policy.length[1]
    }
  }
  # Let's get to work!
  dtmin <- min(in.yr$effdt, in.rc$effdt)
  if (in.year.type == "P"){
    dtmax <- max(addmonths(max(in.rc$effdt), 2 * in.policy.length), addmonths(max(in.yr$expdt), in.policy.length))
  } else{
    dtmax <- max(addmonths(max(in.rc$effdt), in.policy.length), in.yr$expdt)
  }
  dtmaxn <- modiff5(dtmin, dtmax)
  df.yr <- in.yr[order(in.yr$effdt), ]
  row.names(df.yr) <- NULL #reset row index
  df.yr$effdtr <- rnddt(df.yr$effdt)
  df.yr$expdtr <- rnddt(df.yr$expdt)
  df.yr$x1 <- modiff5(dtmin, df.yr$effdtr)
  if (in.year.type == "P"){
    df.yr$x2 <- modiff5(dtmin, df.yr$effdtr) + in.policy.length
    df.yr$x3 <- modiff5(dtmin, df.yr$expdtr) + in.policy.length
  } else{
    df.yr$x2 <- modiff5(dtmin, df.yr$effdtr)
    df.yr$x3 <- modiff5(dtmin, df.yr$expdtr)
  }
  df.yr$x4 <- modiff5(dtmin, df.yr$expdtr)
  sp.yr <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(df.yr[1, "x1"], df.yr[1, "x2"], df.yr[1, "x3"], df.yr[1, "x4"], df.yr[1, "x1"]), c(0, 12, 12, 0, 0)))), 1)))
  if (nrow(df.yr) > 1){
    for (i in 2:nrow(df.yr)){
      sp.yr <- rbind(sp.yr, SpatialPolygons(list(Polygons(list(Polygon(cbind(c(df.yr[i, "x1"], df.yr[i, "x2"], df.yr[i, "x3"], df.yr[i, "x4"], df.yr[i, "x1"]), c(0, 12, 12, 0, 0)))), i))))
    }
  }
  # Prepare the rate change data frame
  # First we aggregate to make sure we don't have any duplicates
  if (in.year.type == "C" & in.prem.type == "W"){
    # If we are working with calendar year written premium then there is no
    # difference between a rate change and a law change so do not include
    # type in the aggregation.
    df.rc <- aggregate(1 + rc ~ effdt, data = in.rc, prod)
    df.rc$type <- "R"
    names(df.rc) <- c("effdt", "rl", "type")
    df.rc <- df.rc[order(df.rc$effdt, df.rc$type), ]
  } else{
    # Otherwise, we aggregate by effdt and type
    df.rc <- aggregate(1 + rc ~ effdt + type, data = in.rc, prod)
    names(df.rc) <- c("effdt", "type", "rl")
    df.rc <- df.rc[order(df.rc$effdt, df.rc$type), ]
  }
  # We create a dummy zero rate change that covers the entire area.
  # This is needed in case the upper left corner of the first historical period
  # does not have a rate change associated with it.  This can become complicated
  # if the first rate change occurs before the first historical period starts
  # and it's a law change or when we are working with calendar year written
  # premiums (which mimics a law change).
  if (!(((in.year.type == "C" & in.prem.type == "W") || df.rc[1, "type"] == "L") & min(in.rc$effdt) <= min(in.yr$effdt))){
    df.rc <- rbind(data.frame(effdt = dtmin, type = "X", rl = 1), df.rc)
  }
  row.names(df.rc) <- NULL #reset row index
  if (in.year.type == "C" & in.prem.type == "W"){
    df.rc$expdt <- df.rc$effdt
  }else{
    df.rc$expdt <- addmonths(df.rc$effdt, ifelse(df.rc$type != "R", 0, in.policy.length))
  }
  df.rc$effdtr <- rnddt(df.rc$effdt)
  df.rc$expdtr <- rnddt(df.rc$expdt)
  df.rc$x1 <- modiff5(dtmin, df.rc$effdtr)
  df.rc$x2 <- modiff5(dtmin, df.rc$expdtr)
  sp.rc <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(df.rc[1, "x1"], df.rc[1, "x2"], dtmaxn, dtmaxn, df.rc[1, "x1"]), c(0, 12, 12, 0, 0)))), 1)))
  if (nrow(df.rc) > 1){
    for (i in 2:nrow(df.rc)){
      sp.rc <- rbind(sp.rc, SpatialPolygons(list(Polygons(list(Polygon(cbind(c(df.rc[i, "x1"], df.rc[i, "x2"], dtmaxn, dtmaxn, df.rc[i, "x1"]), c(0, 12, 12, 0, 0)))), i))))
    }
  }
  sp.rc <- rbind(sp.rc, SpatialPolygons(list(Polygons(list(Polygon(cbind(c(dtmaxn, dtmaxn, dtmaxn, dtmaxn, dtmaxn), c(0, 12, 12, 0, 0)))), i + 1))))
  s <- gDifference(sp.rc[1], gUnaryUnion(sp.rc[2:length(sp.rc)]))
  z <- SpatialPolygonsDataFrame(s, data.frame(arl = df.rc[1, "rl"]), match.ID = F)
  for (i in 2:(length(sp.rc) - 1)){
    m <- length(gDifference(sp.rc[i], sp.rc[1:(i - 1)], byid = T))
    if (m == 0){
      s <- gDifference(gUnaryUnion(sp.rc[i:length(sp.rc)]), gUnaryUnion(sp.rc[(i + 1):length(sp.rc)]))
      t <- s@polygons[[1]]@Polygons
      u <- SpatialPolygons(list(Polygons(t, length(z) + 1)))
      z <- rbind(z, SpatialPolygonsDataFrame(u, data.frame(arl = prod(df.rc[1:i, "rl"])), match.ID = F))
    }else{
      s <- gDifference(sp.rc[i], gUnaryUnion(sp.rc[setdiff((i-m):length(sp.rc), i)]))
      t <- s@polygons[[1]]@Polygons
      u <- SpatialPolygons(list(Polygons(t, length(z) + 1)))
      z <- rbind(z, SpatialPolygonsDataFrame(u, data.frame(arl = prod(df.rc[c(1:(i - m - 1), i), "rl"])), match.ID = F))
      if (m > 1){
        for (j in 1:(m - 1)){
          s <- gIntersection(sp.rc[i], gDifference(gUnaryUnion(sp.rc[setdiff((i - m + j - 1):length(sp.rc), i)]), gUnaryUnion(sp.rc[setdiff((i - m + j):length(sp.rc), i)])))
          t <- s@polygons[[1]]@Polygons
          u <- SpatialPolygons(list(Polygons(t, length(z) + 1)))
          z <- rbind(z, SpatialPolygonsDataFrame(u, data.frame(arl = prod(df.rc[c(1:(i - m + j - 1), i), "rl"])), match.ID = F))
        }
      }
      s <- gDifference(gIntersection(sp.rc[i], sp.rc[i - 1]), gUnaryUnion(sp.rc[(i + 1):length(sp.rc)]))
      t <- s@polygons[[1]]@Polygons
      u <- SpatialPolygons(list(Polygons(t, length(z) + 1)))
      z <- rbind(z, SpatialPolygonsDataFrame(u, data.frame(arl = prod(df.rc[1:i, "rl"])), match.ID = F))
    }
  }
  out.areas <- data.frame(ID.y = as.numeric(),
                          ID.r = as.numeric(),
                          area = as.numeric(),
                          rl = as.numeric())
  out.shapes <- gIntersection(sp.yr[1], z, byid = T, drop_lower_td = T)
  if (length(sp.yr) > 1){
    for (i in 2:length(sp.yr)){
      out.shapes <- rbind(out.shapes, gIntersection(sp.yr[i], z, byid = T))
    }
  }
  for (j in 1:length(out.shapes)){
    h <- out.shapes@polygons[[j]]@ID
    h1 <- as.numeric(substr(h, 1, regexpr(" ", h)[1] - 1))
    h2 <- as.numeric(substr(h, regexpr(" ", h)[1] + 1, nchar(h)))
    w <- data.frame(ID.y = h1,
                  ID.r = h2,
                  area = out.shapes@polygons[[j]]@area,
                  rl = z@data[h2, ],
                  row.names = h)
    out.areas <- rbind(out.areas, w)
  }
  out.areas$warea <- out.areas$area * out.areas$rl
  out.spdf <- SpatialPolygonsDataFrame(out.shapes, out.areas["rl"], match.ID = T)
  out.olfs <- aggregate(out.areas[, c("warea", "area")], by = list(out.areas$ID.y), FUN = sum)
  out.olfs$arl <- out.olfs$warea / out.olfs$area
  names(out.olfs) <- c("ID.y", "warea", "area", "arl")
  out.olfs$crl <- prod(df.rc$rl)
  out.olfs$larl <- out.olfs[length(sp.yr), "arl"]
  out.olfs$olf <- out.olfs$crl / out.olfs$arl
  out.olfs$olf2 <- out.olfs$larl / out.olfs$arl
  p <- spplot(out.spdf, scales = list(draw = TRUE), sp.layout = list("sp.polygons", sp.yr, col = "red", lwd = 4, first = F), xlim = sp.yr@bbox[1, ], ylim = sp.yr@bbox[2, ], main = list(label = paste0(ifelse(in.year.type == "C", "Calendar", "Policy"), "-Year ", ifelse(in.prem.type == "E", "Earned", "Written"), " Premium\n", in.policy.length, "-Month Policies"), cex = .8))
  return(list(out.areas, out.shapes, out.olfs, out.spdf, p))
}
