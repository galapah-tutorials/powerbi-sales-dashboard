CSV_PATH = "Z:/projects/tutorials/powerbi-sales-dashboard/data/random/sales.csv"
ID_COL_NAME = "Workgroup"
DELIMITER = ";"
HEADER_ROW_NR = 3

library(csv)
library(data.table)


#' Decompose a complex data frame into a list of subdataframes
#' 
#' Breaks the input data.frame into smaller data.frames by specified row numbers
#'
split.data.frame.by.rows <- function(df, sep_rows, sub_df_names=NULL)
{
  start = NULL
  df_list = list()
  
  column_names = df[HEADER_ROW_NR,]
  
  for (i in c(sep_rows, nrow(df)) )
  {
    current_df_id = length(df_list) + 1
    print(i)
    if (is.null(start)) {
      # the first iteration, the line number is a sub-dataframe (sub-df) start
      start = i
    } else {
      # any other iteration, the current line number marks the end of the sub-df
      end = i
      df_list[[current_df_id]] = df[(start+HEADER_ROW_NR):(end-1),]
      names(df_list[[current_df_id]]) = column_names
      # current end is the start of the following sub-df
      start = end
    }
  }
  if (!is.null(sub_df_names)) names(df_list) = sub_df_names
  
  return(df_list)
}


# read the CSV
raw_df = read.csv(CSV_PATH, sep=DELIMITER, dec=",", encoding = "UTF-8", header=F)

# locate rows separating the work groups, i.e. rows whose first field contains
#   the string Workgroup
sep_rows = which(raw_df[, 1] == "Workgroup")

# extract work groups IDs (in the second column)
workgroup_ids = raw_df[sep_rows, 2]

# split the raw data.frame into data.frames by work groups
df_list = split.data.frame.by.rows(raw_df, sep_rows, workgroup_ids)

# combine the sub data.frames into one df with a work group column
output = rbindlist(df_list, idcol=ID_COL_NAME, fill=TRUE)

