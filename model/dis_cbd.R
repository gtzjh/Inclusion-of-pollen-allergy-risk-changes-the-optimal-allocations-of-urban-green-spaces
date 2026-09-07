WGS84_CBD_LOCATIONS <- tibble::tribble(
  ~nearest_cbd, ~lon, ~lat,
  "Beijing Road metro station", 113.2650055556, 23.1216222222,
  "Huacheng Square", 113.3191699299, 23.1203276490
)

calculate_nearest_cbd_distance <- function(
    data,
    cbd_locations = WGS84_CBD_LOCATIONS
) {
  required_site_columns <- c("sub_id", "lon", "lat")
  required_cbd_columns <- c("nearest_cbd", "lon", "lat")

  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop("Site data must be a nonempty data frame.", call. = FALSE)
  }
  if (!all(required_site_columns %in% names(data))) {
    stop("Site data must contain sub_id, lon, and lat columns.", call. = FALSE)
  }
  if (anyNA(data$sub_id) || anyDuplicated(data$sub_id)) {
    stop("Site sub_id values must be complete and unique.", call. = FALSE)
  }
  if (!is.numeric(data$lon) || !is.numeric(data$lat)) {
    stop("Site lon and lat columns must be numeric.", call. = FALSE)
  }
  if (!all(is.finite(data$lon)) || !all(is.finite(data$lat))) {
    stop("Site lon and lat values must be finite.", call. = FALSE)
  }
  if (!all(data$lon >= -180 & data$lon <= 180) ||
      !all(data$lat >= -90 & data$lat <= 90)) {
    stop("Site coordinates must be legal WGS84 longitude and latitude values.", call. = FALSE)
  }

  if (!is.data.frame(cbd_locations) ||
      !all(required_cbd_columns %in% names(cbd_locations)) ||
      nrow(cbd_locations) < 1L) {
    stop(
      "CBD configuration must contain nearest_cbd, lon, and lat columns and at least one row.",
      call. = FALSE
    )
  }
  if (!is.character(cbd_locations$nearest_cbd) ||
      anyNA(cbd_locations$nearest_cbd) ||
      any(cbd_locations$nearest_cbd == "") ||
      anyDuplicated(cbd_locations$nearest_cbd)) {
    stop("CBD names must be nonempty and unique.", call. = FALSE)
  }
  if (!is.numeric(cbd_locations$lon) || !is.numeric(cbd_locations$lat)) {
    stop("CBD lon and lat columns must be numeric.", call. = FALSE)
  }
  if (!all(is.finite(cbd_locations$lon)) ||
      !all(is.finite(cbd_locations$lat))) {
    stop("CBD lon and lat values must be finite.", call. = FALSE)
  }
  if (!all(cbd_locations$lon >= -180 & cbd_locations$lon <= 180) ||
      !all(cbd_locations$lat >= -90 & cbd_locations$lat <= 90)) {
    stop("CBD coordinates must be legal WGS84 longitude and latitude values.", call. = FALSE)
  }

  site_coordinates <- as.matrix(data[c("lon", "lat")])
  distance_matrix_m <- matrix(
    vapply(
      seq_len(nrow(cbd_locations)),
      function(cbd_index) {
        geosphere::distGeo(
          site_coordinates,
          c(cbd_locations$lon[cbd_index], cbd_locations$lat[cbd_index])
        )
      },
      numeric(nrow(data))
    ),
    nrow = nrow(data),
    ncol = nrow(cbd_locations)
  )
  nearest_cbd_index <- max.col(-distance_matrix_m, ties.method = "first")

  tibble::tibble(
    sub_id = data$sub_id,
    nearest_cbd = cbd_locations$nearest_cbd[nearest_cbd_index],
    DIS_CBD = distance_matrix_m[
      cbind(seq_len(nrow(data)), nearest_cbd_index)
    ] / 1000
  )
}
