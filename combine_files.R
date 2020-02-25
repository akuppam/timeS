# ######################################
# 4
# combine csv files that have MAPEs
# create a column with region/marketing
# ######################################
setwd(path)
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
# ----------------------------------------------------
library(stringr)
temp <- as.data.frame(str_split_fixed(df_all_mapes$X.ts_model.mape, ",", 3))
library(varhandle)
df_all_mapes <- data.frame(df_all_mapes$folder, temp$V2, unfactor(temp$V3))
names(df_all_mapes) <- c("Region_Marketing","Model","MAPE")
write.csv(df_all_mapes, paste(path, "df_all_mapes.csv"))
# ----------------------------------------------------
# pick the min MAPE for each region_marketing
temp_mape <- df_all_mapes %>%
  group_by(Region_Marketing) %>%
  summarise(MAPE = min(MAPE))
df_min_mapes <- merge(temp_mape, df_all_mapes, by=c("Region_Marketing","MAPE"))
write.csv(df_min_mapes, paste(path, "df_min_mapes.csv"))
# ----------------------------------------------------
# RUN the model again
# pick the min-MAPE-model from 'df_all' using 'df_min_mapes' where Model = df_all$<< min MAPE Model >>



