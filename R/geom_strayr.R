#' @title Create map of Australia with GCCSA capital city insets
#'
#' @description
#' Creates a choropleth of Australia at a chosen ABS Statistical Area level (SA1–SA4),
#' merges user data, applies a continuous or discrete fill, and adds zoomed capital-city
#' insets. Recommended that a user ggsave() the plot, as a plot pane preview
#' does not match the actual output.
#'
#' @param data A data.frame containing a join column and a value column.
#' @param sa_level One of `"sa1"`, `"sa2"`, `"sa3"`, `"sa4"`. Default `"sa3"`.
#' @param year ASGS year (e.g., `2021`). Default `2021`.
#' @param value_col Name of the column in `data` containing mapped values. Default `"your_data"`.
#' @param join_col Name of the geographic join column in `data`. If `NULL`, the function
#'        attempts `<sa_level>_code_<year>` then `<sa_level>_name_<year>`.
#' @param type `"auto"` (default), `"continuous"` or `"discrete"` fill behaviour.
#' @param palette Optional vector of colours. Defaults: `RColorBrewer::brewer.pal(8, "Set3")`.
#' @param keep Simplification ratio passed to `rmapshaper::ms_simplify()`. Default `0.1`.
#' @param title Plot title. Default `"Inset map"`.
#' @param border_colors Named character vector of stroke colours with names
#'        `c("sa","state","gcc")`. Use `NA` to suppress a layer. Default
#'        `c(sa = NA, state = "grey70", gcc = "white")`.
#' @param border_widths Named numeric vector of stroke widths for the same layers.
#'        Default `c(sa = 0, state = 0.1, gcc = 0.1)`.
#'
#' @returns A `ggplot` object composed with `cowplot::ggdraw()` (main map + insets).
#'
#' @examples \dontrun{
#' sa3 <- strayr::read_absmap("sa32021")
#' set.seed(123)
#' fake <- sa3 |>
#'   sf::st_drop_geometry() |>
#'   select(sa3_code_2021) |>
#'   mutate(value = runif(n(), 1, 100))
#'
#' p <- geom_strayr(
#'   data           = fake,
#'   sa_level       = "sa3",
#'   year           = 2021,
#'   join_col       = "sa3_code_2021",
#'   value_col      = "value",
#'   border_colors  = c(sa = NA, state = "black", gcc = "grey80"),
#'   border_widths  = c(sa = 0,  state = 0.4,     gcc = 0.2),
#'   title          = "Demo SA3 map with insets"
#' )
#'
#' # Save with ggsave
#' ggsave("sa3_inset_map.png", p, width = 12, height = 9, dpi = 300)
#' }
#'
#' @export
geom_strayr <- function(
    data,
    sa_level       = "sa3",
    year           = 2021,
    value_col      = "your_data",
    join_col       = NULL,
    type           = c("auto", "continuous", "discrete"),
    palette        = NULL,
    keep           = 0.1,
    title          = "Inset map",
    border_colors  = c(sa = NA, state = "grey70", gcc = "white"),
    border_widths  = c(sa = 0,  state = 0.1,      gcc = 0.1)
) {

  sa_level <- tolower(sa_level)
  stopifnot(sa_level %in% c("sa1","sa2","sa3","sa4"))
  stopifnot(length(year) == 1)

  sa_data_name     <- paste0(sa_level, year)   # e.g. "sa32021"
  state_data_name  <- paste0("state", year)    # "state2021"
  gcc_data_name    <- paste0("gcc",   year)    # "gcc2021"

  code_col_default <- paste0(sa_level, "_code_", year)
  name_col_default <- paste0(sa_level, "_name_", year)

  # --- border config (merge provided with defaults) ---------------------------
  default_border_colors <- c(sa = NA, state = "grey70", gcc = "white")
  default_border_widths <- c(sa = 0,  state = 0.1,      gcc = 0.1)

  border_colors_final <- default_border_colors
  border_colors_final[names(border_colors)] <- border_colors

  border_widths_final <- default_border_widths
  border_widths_final[names(border_widths)] <- border_widths

  # --- quiet read_absmap wrapper ---------------------------------------------
  quiet_read <- function(x) {
    suppressMessages(obj <- strayr::read_absmap(x))
    message("Reading geometry: ", x, " (using cache or downloading as needed)")
    obj
  }

  # --- geometry ---------------------------------------------------------------
  sa_geo    <- quiet_read(sa_data_name)
  state_geo <- quiet_read(state_data_name)
  gcc_geo   <- quiet_read(gcc_data_name)

  sa_geo    <- rmapshaper::ms_simplify(sa_geo,    keep = keep, keep_shapes = TRUE)
  state_geo <- rmapshaper::ms_simplify(state_geo, keep = keep, keep_shapes = TRUE)
  gcc_geo   <- rmapshaper::ms_simplify(gcc_geo,   keep = keep, keep_shapes = TRUE)

  # --- join user data ---------------------------------------------------------
  if (is.null(join_col)) {
    join_col <- if (code_col_default %in% names(data)) code_col_default else
      if (name_col_default %in% names(data)) name_col_default else
        stop("Could not infer `join_col`. Provide '", code_col_default,
             "' or '", name_col_default, "'.")
  }
  if (!value_col %in% names(data)) stop("`value_col` '", value_col, "' not in `data`.")
  if (!join_col %in% names(data))  stop("`join_col` '", join_col,  "' not in `data`.")

  stats_df <- data |>
    dplyr::select(
      !!join_col  := dplyr::all_of(join_col),
      !!value_col := dplyr::all_of(value_col)
    ) |>
    dplyr::distinct()

  # choose geometry-side key to join
  by_cols <- if (join_col %in% names(sa_geo)) {
    stats::setNames(join_col, join_col)
  } else if (code_col_default %in% names(sa_geo)) {
    stats::setNames(join_col, code_col_default)
  } else if (name_col_default %in% names(sa_geo)) {
    stats::setNames(join_col, name_col_default)
  } else {
    stop("Could not align `join_col` to geometry columns.")
  }

  # harmonise types for safe join
  left_name  <- names(by_cols)[1]
  right_name <- unname(by_cols[1])
  stats_df[[left_name]] <- as.character(stats_df[[left_name]])
  sa_geo[[right_name]]  <- as.character(sa_geo[[right_name]])

  sa_map <- dplyr::left_join(sa_geo, stats_df, by = by_cols)

  # --- scale selection --------------------------------------------------------
  type <- match.arg(type)
  if (type == "auto")
    type <- if (is.numeric(stats_df[[value_col]])) "continuous" else "discrete"

  if (is.null(palette))
    palette <- if (type == "continuous") RColorBrewer::brewer.pal(8, "Set3")

  fill_scale <- if (type == "continuous") {
    ggplot2::scale_fill_gradientn(colours = palette)
  } else {
    ggplot2::scale_fill_manual(values = palette)
  }

  # --- national bounds ----------
  nat_xlim <- c(108, 166)
  nat_ylim <- c(-46,  -7)

  # --- main map (title on main only) -----------------------------------------
  map_base <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = sa_map, ggplot2::aes(fill = .data[[value_col]]),
      colour = border_colors_final[["sa"]],
      linewidth = border_widths_final[["sa"]]
    ) +
    ggplot2::theme_void() +
    ggplot2::xlim(nat_xlim) +
    ggplot2::ylim(nat_ylim) +
    ggplot2::coord_sf(
      xlim = nat_xlim,
      ylim = nat_ylim,
      expand = FALSE
    ) +
    ggplot2::geom_sf(
      data = gcc_geo,   colour = border_colors_final[["gcc"]],
      linewidth = border_widths_final[["gcc"]], fill = NA
    ) +
    ggplot2::geom_sf(
      data = state_geo, colour = border_colors_final[["state"]],
      linewidth = border_widths_final[["state"]], fill = NA
    ) +
    ggplot2::labs(title = title, fill = "") +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(hjust = 0.5),
      legend.position  = c(0.5, 0.95),
      legend.direction = "horizontal"
    ) +
    fill_scale

  # --- inset specs ------------------------------------------------------------
  gcc_insets <- tibble::tribble(
    ~gcc_name,   ~xmin,    ~xmax,    ~ymin,    ~ymax,    ~xpos,  ~ypos,  ~scale,
    "Sydney",     150.5,    151.5,   -34.4,    -33.4,     0.72,   0.41,   0.25,
    "Melbourne",  144.35,   145.90,  -38.60,   -37.25,    0.704,  0.01,   0.20,
    "Brisbane",   152.60,   153.73,  -28.20,   -26.70,    0.731,  0.665,  0.20,
    "Adelaide",   138.40,   139.10,  -35.30,   -34.50,    0.215,  0.01,   0.35,
    "Perth",      115.45,   116.50,  -32.80,   -31.40,    0.070,  0.01,   0.20,
    "Hobart",     147.00,   148.00,  -43.20,   -42.60,    0.361,  0.01,   0.30,
    "Darwin",     130.80,   131.40,  -12.90,   -12.00,    0.635,  0.739,  0.25,
    "Canberra",   148.85,   149.43,  -35.50,   -35.05,    0.736,  0.295,  0.25
  )

  # selection rectangles on main map (also appear in insets)
  map_aus <- map_base +
    ggplot2::annotate(
      "rect",
      xmin = gcc_insets$xmin, xmax = gcc_insets$xmax,
      ymin = gcc_insets$ymin, ymax = gcc_insets$ymax,
      fill = NA, colour = "grey30", linewidth = 0.6
    )

  # create a title-less clone for insets so they don't repeat the main title
  map_aus_inset <- map_aus +
    ggplot2::labs(title = NULL) +
    ggplot2::theme(plot.title = ggplot2::element_blank())

  # Build each inset; suppress the coord replacement message for a clean console
  build_inset <- function(g_row) {
    suppressMessages(
      map_aus_inset +
        ggplot2::coord_sf(
          xlim = c(g_row$xmin, g_row$xmax),
          ylim = c(g_row$ymin, g_row$ymax),
          expand = FALSE
        )
    ) +
      ggplot2::labs(subtitle = g_row$gcc_name) +
      ggplot2::theme(
        legend.position = "none",
        plot.subtitle   = ggplot2::element_text(face = "bold", hjust = 0.5, size = 9,
                                                margin = ggplot2::margin(b = 2))
      )
  }

  inset_layers <- lapply(seq_len(nrow(gcc_insets)), function(i) {
    g <- gcc_insets[i, ]
    cowplot::draw_plot(
      build_inset(g),
      x = g$xpos, y = g$ypos,
      width  = (g$xmax - g$xmin) * g$scale,
      height = (g$ymax - g$ymin) * g$scale
    )
  })

  p_out <- cowplot::ggdraw(map_aus)
  for (lay in inset_layers) p_out <- p_out + lay

  # friendly completion message
  message("Inset map made 🗺️")

  return(p_out)
}
