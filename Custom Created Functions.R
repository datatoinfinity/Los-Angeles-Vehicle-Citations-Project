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
  output
}

graph_cits <- function(my_data, plot_title, x_axis_name, y_axis_name,
                       x_axis_label, y_axis_label) {
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
  #   x_axis_label: A string which is the name of the x-axis label on the plot.
  #   y_axis_label: A string which is the name of the y-axis label on the plot.
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
  if (is.character(x_axis_label) == FALSE) {
    stop("Argument x_axis_label is of incorrect data type:", str(x_axis_name),
         ".")
  }
  if (is.character(y_axis_label) == FALSE) {
    stop("Argument y_axis_label is of incorrect data type:", str(y_axis_name),
         ".")
  }
  ggplot(my_data,
         aes_string(x = x_axis_name, y = y_axis_name)) +
    geom_bar(color = "red",
             fill = "red",
             stat = "identity") + 
    theme(axis.text.x = element_text(hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom") +
    ggtitle(plot_title) +
    labs(x = x_axis_label,
         y = y_axis_label)
}

make_map <- function(my_data, loc_map, lat_col_name, long_col_name,
                     plot_title) {
  # Makes a map of a given area based on longitude and latitude coordinates.
  #
  # Args:
  #   my_data: A dataframe with columns of latitude and longitude coordinates
  #            and Frequency of each pair of coordinates.
  #   loc_map: A ggmap object that is a longitude, latitude map of interest
  #            to superimpose a ggplot on.
  #   lat_col_name: The string name of the column containing the latitude
  #                  coordinates.  
  #   long_col_name: The string name of the column containing the longitude
  #                  coordinates.
  #   plot_title: The string name of the title of the plot.
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
  if (is.character(plot_title) == FALSE) {
    stop("Argument plot_title is of incorrect data type: ",
         str(plot_title), ".")
  }   
  ggmap(loc_map) +
    stat_density2d(data = my_data,
                   aes_string(x = long_col_name, y = lat_col_name,
                              fill = "..density.."),
                   geom = 'tile',
                   contour = FALSE,
                   alpha = .4) +
    scale_fill_viridis(option = 'inferno') +
    labs(title = str_c(plot_title),
         fill = str_c('Number of', '\nCitations'),
         x = "Longitude",
         y = "Latitude") +
    theme(text = element_text(color = "#444444"),
          plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))
}