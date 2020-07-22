# Download data from OSF
# ----------------------
ininstall.packages('osfr')
library(osfr)

# Get tibble of data on the OSF store
data <- osfr::osf_ls_files(osf_retrieve_node("c7kg4"))

# Download to ./data
dir.create("./data")
osf_download(data, path="./data", verbose=TRUE, progress=TRUE, recurse=TRUE, conflicts="overwrite")
