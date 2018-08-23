tep <- function(idata, verbose = FALSE, path = NULL, fl = 128L, ovlp = 50L, offset = 70L) {
  ## primary sanity check
  if (missing(idata)) {
    stop("Argument 'idata' must be specified.")
  } else {
    if (!is.matrix(idata)) {
      stop("Argument 'idata' must be a matrix.")
    } else {
      if (!is.integer(idata)) {
        stop("Argument 'idata' must be an integer matrix.")
      } else {
        if (any(idata!=0L & idata!=1L)) {
          stop("Argument 'idata' must be a binary matrix.")
        } else {
            if (ncol(idata)!=20) {
              stop("Argument 'idata' must have 20 columns.")
            }
          }
      }
    }
  }
  if (!is.logical(verbose)) {
    stop("Argument 'verbose' must be logical (TRUE ou FALSE).")
  }
  retvals <- .Fortran("temain",
                      npts = as.integer(60*3*nrow(idata)),
                      nx = nrow(idata),
                      idata = idata,
                      xdata = as.double(matrix(0, nrow(idata), 52)),
                      verbose = as.integer(verbose))
  retvals$xdata <- matrix(retvals$xdata, nrow(idata), 52)
  colnames(retvals$xdata) <- c(paste('XMEAS(', 1:41, ")", sep = ""), paste('XMV(', 1:11, ")", sep = ""))

  if (!is.null(path)) {

    aux33 <- retvals$xdata
    tl <- retvals$nx

    if ( tl %% fl*(ovlp/100) != 0 ) {
      stop("Number of rows of 'idata' must be multiple of fl*(ovlp/100).")
      } else {
        if ( tl/(fl*(ovlp/100)) < 3 ) {
          stop("Number of rows of 'idata' must be greater than 3 times fl*(ovlp/100).")
        }
      }

    train <- file.path(path, 'input', 'tep_dataset', 'train')
    test  <- file.path(path, 'input', 'tep_dataset', 'test')

    if (!dir.exists(train)) {
      dir.create(train, recursive = TRUE)
    }
    if (!dir.exists(test)) {
      dir.create(test, recursive = TRUE)
    }


    for (i in 1:ncol(aux33)) {
      aux44 <- scale(aux33[, i])
      aux55 <- cbind(matrix(aux44, tl/(fl*(ovlp/100)),fl*(ovlp/100), byrow = TRUE)[1:(tl/(fl*(ovlp/100))-1), ],
                     matrix(aux44, tl/(fl*(ovlp/100)),fl*(ovlp/100), byrow = TRUE)[2:(tl/(fl*(ovlp/100))), ])
      ff <- paste(colnames(aux33)[i], ".txt", sep = "")
      setwd(train)
      write.table(aux55[1:as.integer((offset/100)*nrow(aux55)), ], file = file(ff), row.names = FALSE, col.names = FALSE)
      setwd(test)
      write.table(aux55[-(1:as.integer((offset/100)*nrow(aux55))), ], file = file(ff), row.names = FALSE, col.names = FALSE)
    }
    aux66 <- apply(retvals$idata, 1, function(x) { if(any(x==1)) { which(x==1) } else { 0L } })
    aux77 <- cbind(matrix(aux66, tl/(fl*(ovlp/100)),fl*(ovlp/100), byrow = TRUE)[1:(tl/(fl*(ovlp/100))-1), ],
                   matrix(aux66, tl/(fl*(ovlp/100)),fl*(ovlp/100), byrow = TRUE)[2:(tl/(fl*(ovlp/100))), ])
    aux77 <- apply(aux77, 1, function(x) {
      z <- table(x)
      as.integer(names(z)[z == max(z)][1])
    })

    ff <- "idv.txt"
    setwd(train)
    write.table(aux77[1:as.integer((offset/100)*NROW(aux77))], file = file(ff), row.names = FALSE, col.names = FALSE)
    setwd(test)
    write.table(aux77[-(1:as.integer((offset/100)*NROW(aux77)))], file = file(ff), row.names = FALSE, col.names = FALSE)
    setwd(path)

    sprintf("The LSTM formated data was saved in", file.path(path, 'input'))

  }

  return(retvals$xdata)
}


tepAttrib <- function(i = 1:52) {
  ## primary sanity check
  if (!is.integer(i)) {
    stop("Argument 'i' must be an integer.")
  } else {
    if (any(i < 1) | any(i > 53)) {
      stop("Elements of 'i' must be integers between 1 and 53.")
    }
  }
  description <- c(
    'A Feed (stream 1)',
    'D Feed (stream 2)',
    'E Feed (stream 3)',
    'A and C Feed (stream 4)',
    'Recycle Flow (stream 8)',
    'Reactor Feed Rate',
    'Reactor Pressure',
    'Reactor Level',
    'Reactor Temperature',
    'Purge Rate (stream 9)',
    'Product Sep Temp',
    'Product Sep Level',
    'Prod Sep Pressure',
    'Prod Sep Underflow (stream 10)',
    'Stripper Level',
    'Stripper Pressure',
    'Stripper Underflow (stream 11)',
    'Stripper Temperature',
    'Stripper Steam Flow',
    'Compressor Work',
    'Reactor Cooling Water Outlet Temp',
    'Separator Cooling Water Outlet Temp',
    'Component A (stream 6)',
    'Component B (stream 6)',
    'Component C (stream 6)',
    'Component D (stream 6)',
    'Component E (stream 6)',
    'Component F (stream 6)',
    'Component A (stream 9)',
    'Component B (stream 9)',
    'Component C (stream 9)',
    'Component D (stream 9)',
    'Component E (stream 9)',
    'Component F (stream 9)',
    'Component G (stream 9)',
    'Component H (stream 9)',
    'Component D (stream 11)',
    'Component E (stream 11)',
    'Component F (stream 11)',
    'Component G (stream 11)',
    'Component H (stream 11)',
    'D Feed Flow (stream 2)',
    'E Feed Flow (stream 3)',
    'A Feed Flow (stream 1)',
    'A and C Feed Flow (stream 4)',
    'Compressor Recycle Valve',
    'Purge Valve (stream 9)',
    'Separator Pot Liquid Flow (stream 10)',
    'Stripper Liquid Product Flow (stream 11)',
    'Stripper Steam Valve',
    'Reactor Cooling Water Flow',
    'Condenser Cooling Water Flow',
    'Agitator Speed'
  )
  unit <- c(
    'kscmh',
    'kg h-1',
    'kg h-1',
    'kscmh',
    'kscmh',
    'kscmh',
    'kPa',
    '%',
    'oC',
    'kscmh',
    'oC',
    '%',
    'kPa',
    'm3 h-1',
    '%',
    'kPa',
    'm3 h-1',
    'oC',
    'kg h-1',
    'kW',
    'oC',
    'oC',
    rep('mole %', 19),
    rep('%', 12)
  )
  variable <- c(paste('XMEAS(', 1:41, ")", sep = ""), paste('XMV(', 1:12, ")", sep = ""))
  out <- cbind(variable, description, unit)

  return(out[i, ])
}
