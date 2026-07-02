#' @description `mpaviewer` is an interactive dashboard for
#' Western Australian marine monitoring data,
#' hosted at <https://mpaviewer.dbca.wa.gov.au/>.
#'
#' @keywords internal
"_PACKAGE"

# CMD check silencer
utils::globalVariables(
  c(
    ".", "..density..", "abundance", "actionBttn", "addCircleMarkers", "addGlPolygons",
    "addLayersControl", "addLegend", "addMarkers", "addTiles", "aes", "annotation_custom",
    "arrange", "bind_rows", "box", "campaignid", "colorFactor", "cols", "coord_flip",
    "dashboardBody", "dashboardHeader", "dashboardPage", "dashboardSidebar", "data.dir",
    "depth", "desc", "distinct", "element_blank", "element_text", "expand_scale",
    "facet_grid", "facet_wrap", "family", "fct_relevel", "filename", "filter", "fishing.type",
    "fitBounds", "folder.structure", "frame", "genus", "geom_bar", "geom_density",
    "geom_label", "geom_vline", "ggplot", "ggtitle", "glimpse", "gpar", "grobTree", "group_by",
    "hideGroup", "latitude", "layersControlOptions", "leaflet", "leafletOutput",
    "location", "longitude", "marine.park", "maxn", "menuItem", "method", "metric", "mutate", "n",
    "nesting", "number", "number.of.times.sampled", "opcode", "park.popups", "period",
    "periodtime", "pickerInput", "position_dodge", "pull", "read.csv", "readOGR",
    "renderLeaflet", "reorder", "rls.trophic.group", "scale_x_continuous",
    "scale_y_continuous", "scientific", "sd", "shinyalert", "sidebarMenu", "site", "slice",
    "species", "stat_smooth", "stat_summary", "status", "str_replace_all", "tabBox",
    "tabItem", "tabItems", "textGrob", "theme", "top_n", "total", "total.abundance",
    "trophic.group", "ungroup",
    "useShinyalert", "value", "withSpinner", "xlab", "year", "year.zoned", "ylab",
    "mpa_data", "mean_lat", "marine_park", "scientific_name", "depth_m",
    "number_of_times_sampled", "plot_year", "percent_cover", "sector",
    "latitude_dd", "longitude_dd", "reef_zone", "folder_structure",
    "indicator", "zone.1", "agency", "dbca_zone", "date_time",
    "successful_count", "successful_length", "observer_count",
    "dbca_sanctuary", "full.name", "file_full_dir", "X6", "park",
    "park_path", "method_path", "level2class", "Level3Class",
    "park_family", "park_site", "survey", "level3class", "zone",
    "site_code", "analysis", "software", "site_depth", "pics",
    "australian_common_name", "complex_functional_group",
    "simple_functional_group", "include_years", "include_sites",
    "label", "report.as", "scientific_name_label", "taxa",
    "complete_needed", "complete", "gazetted", "re_zoned",
    "length_mm", "genus_correct", "species_correct",
    "family_correct", "species_key", "id", "report_as",
    "trophic_group", "maxnlog", "total_cti", "mean_cti",
    "total_maxn", "transect", "campaignstatus", "med_length",
    "global_region", "marine_region", "marine_region_mass",
    "fb_a", "fb_b", "fb_b_ll", "fb_length_weight_measure",
    "fb_a_ll", "length.cm", "adjlength", "biomass.g",
    "biomass.kg", "mins.per.sample", "cti", "legend",
    "coral_cover", "cover"
  )
)
