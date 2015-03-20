
#' @export
read_aco <- function(path, .verbose=FALSE) {

  if (is_url(path)) {
    tf <- tempfile()
    stop_for_status(GET(path, write_disk(tf)))
    path <- tf
  }

  aco <- readBin(path.expand(path), "raw",
                file.info(path.expand(path))$size, endian="big")

  version <- unpack("v", aco[2:1])[[1]]
  n_colors <- unpack("v", aco[4:3])[[1]]

  if (.verbose) {
    message("Version: ", version)
    message("# Colors: " , n_colors)
  }

  if (version == 1) {
    decode_aco_v1(aco, n_colors)
  } else {
    message("ACO v2 not supported yet")
    return(NULL)
  }

}

#' @export
read_ase <- function(path, .verbose=FALSE) {

  if (is_url(path)) {
    tf <- tempfile()
    stop_for_status(GET(path, write_disk(tf)))
    path <- tf
  }

  ase <- readBin(path, "raw",
                 file.info(path.expand(path))$size, endian="big")

  if (unpack("A4", ase[1:4])[[1]] != "ASEF") {
    message("Not a valid ASE file")
    stop()
  }

  version <- sprintf("%d.%d", b2i(ase[5:6]), b2i(ase[7:8]))

  block_count <- b2li(ase[9:12])

  if (.verbose) {
    message("ASE Version: ", version)
    message("Block count: ", block_count)
  }

  decode_ase(ase, block_count)

}


#' @export
show_palette <- function(palette) {
  n <- length(palette)
  if (length(palette > 0)) {
    image(1:n, 1, as.matrix(1:n), col = palette,
          xlab = "", ylab = "", xaxt = "n", yaxt = "n",
          bty = "n")

  }
}
