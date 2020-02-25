# ######################################
# combine csv files that have MAPEs
# create a column with region/marketing
# ######################################


common_path = "A:/LogIIS/"
primary_dirs = list.files(common_path);
primary_dirs 
# [1] "FOLDER01" "FOLDER02" "FOLDER03"

for(dir in primary_dirs) {
  sub_folders = list.files(paste(common_path,dir,sep = ""))
  if (any(sub_folders %in% "files.csv")) {
    ## there is files.csv in this directory read it in and append to a data.frame.
    ## read in data 
    temp_data = read.csv(file = paste(common_path,dir,"/files.csv",sep = ""))
    ## append
    main_data = cbind(main_data,temp_data);
  } else {
    ## try go one more directory deeper
    for(sub_dir in sub_folders) {
      sub_sub_files = list.files(paste(common_path,dir,"/",sub_dir,sep = ""))             
      if (any(sub_sub_files %in% "files.csv")) {
        ## found files.csv read it in and append it
        temp_data = read.csv(file = paste(common_path,dir,"/",sub_dir,"/files.csv",sep = ""))
        main_data = cbind(main_data,temp_data);
      } else {
        warning("could not find the file 'files.csv' two directories deep")
      }
    } 
  }
}

# -----------------
# https://stackoverflow.com/questions/43900965/read-several-files-in-different-directories-in-r
# ---------------------------------------------

common_path = "/users/akuppam/documents/Hprog/R/Prophet/"
primary_dirs = list.files(common_path);
primary_dirs
# [1] "FOLDER01" "FOLDER02" "FOLDER03"

main_data = data.frame("main_data")
  
for(dir in primary_dirs) {
  sub_folders = list.files(paste(common_path,dir,sep = ""))
  if (any(sub_folders %in% "files.csv")) {
    ## there is files.csv in this directory read it in and append to a data.frame.
    ## read in data 
    temp_data = read.csv(file = paste(common_path,dir,"/files.csv",sep = ""))
    ## append
    main_data = cbind(main_data,temp_data);
  } else {
    ## try go one more directory deeper
    for(sub_dir in sub_folders) {
      sub_sub_files = list.files(paste(common_path,dir,"/",sub_dir,sep = ""))             
      if (any(sub_sub_files %in% "files.csv")) {
        ## found files.csv read it in and append it
        temp_data = read.csv(file = paste(common_path,dir,"/",sub_dir,"/files.csv",sep = ""))
        main_data = cbind(main_data,temp_data);
      } else {
        warning("could not find the file 'files.csv' two directories deep")
      }
    } 
  }
}
summary(main_data)

# --------------------------
# https://stackoverflow.com/questions/50454735/reading-tsv-files-from-different-folders-into-r-environment-and-adding-a-column?rq=1
# --------------------------
path = "/users/akuppam/documents/Hprog/R/Prophet/Test5wHols/"
setwd(path)
# load the needed packages
library(dplyr)

# create a list of the filenames with the full path
file.list <- list.files(pattern='MAPE_by_model.csv', recursive = TRUE, full.names = TRUE)

# read the files into a list
# using 'simplify=FALSE' makes sure the full paths are used as names in the list
df.list <- sapply(file.list, read.delim, simplify=FALSE)

# bind the dataframes in the list together with 'bind_rows' from the dplyr-package
# use to replace the full path name with the folder name
df_all_mapes <- bind_rows(df.list, .id = "folder") %>%
  mutate(folder = sub('.*/(.*)/.*$', '\\1', folder))
write.csv(df_all_mapes, paste(path, "df_all_mapes.csv"))

library(splitstackshape)
cSplit(dat, "reaction_time", ",")

library(splitstackshape)
cSplit(df, "reaction_time", ",")
# ----------------------------------------------------


