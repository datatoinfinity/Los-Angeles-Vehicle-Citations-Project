# Functions for LA Vehicle Citations Project

correct_time <- function(my_data, column, column_name) {
  # Corrects improperly formatted military time (19 for 12:19 AM, 1345 for 1:45
  # PM, etc.) to format that includes appropriate 0s (19 to 00:19, 13:45, etc.)
  #
  # Args:
  #   my_data: A data_frame with a column that is improperly formatted military
  #            time.
  #   column: A character column of improperly formatted military times.
  #   column_name: The string name of column that is improperly formatted. 
  #
  # Returns:
  #   A vector of characters of reformatted military times.
  # Error handling (rudimentary)
  if (is.data.frame(my_data) == FALSE) {
    stop("Argument my_data is of incorrect data type: ",
         str(my_data), ".")
  }
  if (is.character(column) == FALSE) {
    stop("Argument column is of incorrect data type: ",
         str(column), ".")
  } 
  if (!(column_name %in% names(my_data))) {
    stop("Argument column_name is not the name of a column of my_data.")
  }
  output <- sapply(column, function(x) {
    if (!is.na(x)) {
      if (str_length(x) == 3) {
        y <- paste('0', x, sep = "")
        # Need to insert ':' into middle of string before cast to time.
        z <- strsplit(y, "")[[1]]
        x <- paste0(c(z[1:2], ":", z[3:4]), collapse = '')
      } else if (str_length(x) == 2) {
        x <- paste('00:', x, sep = "")
      } else if (str_length(x) == 1) {
        x <- paste('00:0', x, sep = "")
      } else {
        y <- strsplit(x, "")[[1]]
        x <- paste0(c(y[1:2], ":", y[3:4]), collapse = '')
      }
    } else {
      # To keep NA values from turning into NULL.
      x <- x
    }
  })
  return(output)
}

make_map <- function(my_data, lat_col_name, long_col_name, lat_lims,
                     long_lims, zoom_amt) {
  # Makes a map of a given area based on longitude and latitude coordinates.
  #
  # Args:
  #   my_data: A dataframe with columns of latitude and longitude coordinates
  #            and Frequency of each pair of coordinates.
  #   lat_col_name: The string name of the column containing the latitude
  #                  coordinates.  
  #   long_col_name: The string name of the column containing the longitude
  #                  coordinates.
  #   lat_lims: A vector of 2 indicates that latitude coordinate limits of the 
  #            map.
  #   long_lims: A vector of 2 indicates that longitude coordinate limits of 
  #              the map.
  #   zoom_amt: An integer between 3 and 21 (or 18 depending on the mapping
  #             software) that specifics the level of zoom that you want your
  #             map to have. 3 is continent level, 21 is building level.
  #
  # Returns:
  #   A heat map of a variable of interest at a given location.
  # Error handling (rudimentary)
  if (is.data.frame(my_data) == FALSE) {
    stop("Argument my_data is of incorrect data type: ",
         str(my_data), ".")
  }
  if (!(lat_col_name %in% names(my_data))) {
    stop("Argument lat_col_name is not the name of a column of my_data.")
  }
  if (!(long_col_name %in% names(my_data))) {
    stop("Argument long_col_name is not the name of a column of my_data.")
  }  
  if (length(lat_lims) != 2) {
    stop("Argument lat_lims must have two values: ",
         str(lat_lim), "value currently.")
  }
  if (is.numeric(lat_lims) == FALSE) {
    stop("Argument lat_lims values must only be numeric: ",
         is.numeric(lat_lim), ".")
  }
  if (lat_lims[1] == lat_lims[2]) {
    stop("Argument lat_lims values must be different: ",
         lat_lims, ".")
  }  
  if (length(long_lims) != 2) {
    stop("Argument long_lims must have two values: ",
         str(long_lims), "value currently.")
  }
  if (is.numeric(lat_lims) == FALSE) {
    stop("Argument long_lims values must only be numeric: ",
         is.numeric(long_lims), ".")
  }
  if (long_lims[1] == long_lims[2]) {
    stop("Argument long_lims values must be different: ",
         long_lim, ".")
  }  
  if (is.integer(zoom_amt) == FALSE) {
    stop("Argument zoom_amt must be an integer: ",
         str(zoom_amt), ".")    
  }
  if ((zoom_amt < 3) | (zoom_amt > 21)) {
    stop("Argument zoom_amt must be an integer between 3 and 21: ",
         zoom_amt, ".")    
  }  
  # There is likely a better way using dplyr and quosures, but I was unable
  # to get it to work.
  sub_dat <- my_data[which(my_data[, lat_col_name] >= lat_lims[1]), ] %>%
    .[which(.[, lat_col_name] <= lat_lims[2]), ] %>%
    .[which(.[, long_col_name] >= long_lims[1]), ] %>%
    .[which(.[, long_col_name] <= long_lims[2]), ]
  loc_map <- get_map(location = c(lon = mean(long_lims), lat = mean(lat_lims)),
                     zoom = zoom_amt)
  ggmap(loc_map) +
    stat_density2d(data = sub_dat, aes(x = long, y = lat, fill = ..level..,
                                       alpha = ..level..),
                   geom = "polygon", size = 0.01, bins = 16) +
    scale_fill_gradient(low = "black", high = "red") +
    scale_alpha(range = c(0, 0.3), guide = FALSE)
}

graph_cits <- function(my_data, plot_title, x_axis_name, y_axis_name) {
  # Graphs the daily number of citations for a given month.
  #
  # Args:
  #   my_data: A dataframe with columns of latitude and longitude coordinates
  #            and Frequency of each pair of coordinates.
  #   plot_title: A string which is the desired title of the plot.
  #   x_axis_name: A string which is name of the column in my_data to be used
  #                for the x-axis data.
  #   y_axis_name: A string which is name of the column in my_data to be used
  #                for the y-axis data.
  #
  # Returns:
  #   A bar graph of a daily number of citations for a given month.
  # Error handling (rudimentary)
  if (is.data.frame(my_data) == FALSE) {
    stop("Argument my_data is of incorrect data type: ", str(my_data), ".")
  }
  if (is.character(plot_title) == FALSE) {
    stop("Argument plot_title is of incorrect data type:", str(plot_title),
         ".")
  }
  if (is.character(x_axis_name) == FALSE) {
    stop("Argument x_axis_name is of incorrect data type:", str(x_axis_name),
         ".")
  }
  if (is.character(y_axis_name) == FALSE) {
    stop("Argument y_axis_name is of incorrect data type:", str(y_axis_name),
         ".")
  }
  # e <-
  ggplot(my_data, aes_string(x = x_axis_name, y = y_axis_name)) +
    geom_bar(color = "red", fill = "red", stat = "identity") + 
    theme(axis.text.x = element_text(hjust = 1),
          plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
    ggtitle(plot_title) +
    labs(x = 'Month and Day', y = "# of Citations")
  #print(e)
}