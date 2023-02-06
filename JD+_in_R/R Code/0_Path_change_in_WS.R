library(stringr)

# there is a function based on this program in the rjdworkspace package (github.com/InseeFrLab)
getwd()
# PARAM 1
# new path to workspace: target = SAprocessing file inside workspace to be modified
path_to_ws_SAP<-"Workspaces/industrie_new_path/SAProcessing/SAProcessing-1.xml"
path_to_ws_SAP

# PARAM 2 : new path to data to BE WRITTEN into xml file (old path doesn't matter)
# has to be complete not relative for JD+
# avoid space in path, if spaces replace by +
path_to_raw_data<-"C:/Users/YWYD5I/Documents/ESTP_Beginner_SA/January_2023_Training/Data/IPI_nace4.csv"
#path_to_raw_data<-"C:/Users/YWYD5I/Documents/ESTP_Beginner_SA/January_2023_Training/JD+_in_R/Data/IPI_nace4.csv"
path_to_raw_data

# formatting path in JD+ mode (2 steps)
path_to_raw_data_JD1<-gsub(":","%3A",path_to_raw_data)
path_to_raw_data_JD<-gsub("/","%5C",path_to_raw_data_JD1)
path_to_raw_data_JD

# lecture fichier xml 
xml_file<-readLines(path_to_ws_SAP)
# load raw data contained in sa processing te retrieve the series names
control_data<-read.csv2(path_to_raw_data)
raw_series_names<-colnames(control_data)[-1] # we leave the date out

# for each series (sa item in xml file) we cut out the old path and paste the new

for (i in seq(1,length(raw_series_names))){
  print(raw_series_names[i])
  i_path_to_data<-grep(paste0('"',raw_series_names[i],'"'),xml_file)+8
  chaine1<-unlist(str_split(xml_file[i_path_to_data],"file="))
  chaine2<-unlist(str_split(chaine1[2],".csv"))
  xml_file[i_path_to_data]<-paste0(chaine1[1],"file=",path_to_raw_data_JD,chaine2[2])
  
}
writeLines(xml_file,path_to_ws_SAP) # rewriting file at the same place


