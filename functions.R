

# Get year of date
as.year <- function(x) {
  as.numeric(format(as.Date(x,format="%d/%m/%Y"),"%Y"))
}

# Import excel file of Regex codes for diagnosis, medicine and procedure codes
# Expected columns: Regex, Name and Weight (all other columns will be ignored)
read_code_list <- function(phile) {
  code_list <- readxl::read_excel(phile)
  if (!("Weight" %in% colnames(code_list))) {
    code_list <- code_list |> mutate(Weight = 1)
  }
  code_list <- code_list |> mutate(Weight = ifelse(is.na(Weight),1,Weight)) |> select(Name,Regex,Weight)
  code_list$Name <- make.names(code_list$Name)
  return(code_list)
}

# Make list of string 
deparse_list <- function(list_of_objs) {
  mylist <- rep("",length(list_of_objs))
  for (j in 1:length(mylist)) {
    mylist[[j]] <- deparse(substitute(list_of_objs[[j]]))
  }
  return(mylist)
}

# Dictionary used to standardise variable names
names_dict <- c(
  PNR = "PNR",
  RECNUM = "RECNUM",
  RECNUM = "DW_EK_KONTAKT",
  DIAGNOSEKODE = "C_DIAG",
  DIAGNOSEKODE = "DIAGNOSEKODE",
  DIAGNOSETYPE = "C_DIAGTYPE",
  DIAGNOSETYPE = "DIAGNOSETYPE",
  DATO_START = "DATO_START",
  DATO_START = "D_INDDTO",
  DATO_START = "D_ODTO",
  PROCEDUREKODE = "PROCEDUREKODE",
  PROCEDUREKODE = "C_OPR"
)

# Import and merge two sas files
# Use filter_fct_diag and filter_fct_adm to extract specific diagnoses, procedures etc.
# Use lookup to standardize variable names
readlpr_file_file <- function(x,
                              y,
                              lookup=names_dict,
                              join_by="RECNUM",
                              path = rawg,
                              filter_fct_diag = nofilter, # fct or list of fcts
                              filter_fct_adm = nofilter, # fct or list of fcts
                              keep = NULL,
                              add_filname=FALSE) {
  
  # Is file?
  if (file.exists(paste0(path,x))) {
    df1 <- read_sas(paste0(path,x))
    xref <- "df1"
  } else {
    stop(paste(x,"is not file"))
  }
  
  # Is file?
  if (file.exists(paste0(path,y))) {
    df <- read_sas(paste0(path,y))
    yref <- "df"
  } else {
    stop(paste(y,"is not file"))
  }
  
  # Rename variables
  eval(parse(text=paste(xref,"<-",xref,"%>% rename(any_of(lookup))")))
  eval(parse(text=paste(yref,"<-",yref,"%>% rename(any_of(lookup))")))
  
  # Filter diagnoses
  eval(parse(text=paste(xref,"<-",xref,"%>% filter_fct_diag")))
  
  # Filter adm
  eval(parse(text=paste(yref,"<-",yref,"%>% filter_fct_adm")))
  
  # Join
  eval(parse(text=paste("df <- join(",yref,",",xref,",by=join_by,type='inner',match='all')")))
  
  if (!is.null(keep)) {
    df <- df %>% select(any_of(keep))
  }
  if (add_filname) {
    df <- df %>% mutate(filename = paste(x,y))
  }
  print(paste(".",x,y))
  return(df)
}

# Import sas file and merge with data frame
# Use filter_fct_diag and filter_fct_adm to extract specific diagnoses, procedures etc.
# Use lookup to standardize variable names
readlpr_file_dafr <- function(x,
                              y,
                              lookup=names_dict,
                              join_by="RECNUM",
                              path = rawg,
                              filter_fct_diag = nofilter, # fct or list of fcts
                              filter_fct_adm = nofilter, # fct or list of fcts
                              keep = NULL,
                              add_filname=FALSE) {
  
  # Is file?
  if (file.exists(paste0(path,x))) {
    df <- read_sas(paste0(path,x))
    xref <- "df"
  } else {
    stop(paste(x,"is not file"))
  }
  
  # Is data.frame?
  if (any(class(eval(parse(text=paste0(y)))) %in% c("tbl_df","tbl","data.frame"))) {
    yref <- y
  } else {
    stop(paste(y,"is not data.frame"))
  }
  
  # Rename variables
  eval(parse(text=paste(xref,"<-",xref,"%>% rename(any_of(lookup))")))
  eval(parse(text=paste(yref,"<-",yref,"%>% rename(any_of(lookup))")))
  
  # Filter diagnoses
  eval(parse(text=paste(xref,"<-",xref,"%>% filter_fct_diag")))
  
  # Filter adm
  eval(parse(text=paste(yref,"<-",yref,"%>% filter_fct_adm")))
  
  print(object_size(df))
  
  # Join
  eval(parse(text=paste("df <- join(",yref,",",xref,",by=join_by,type='inner',match='all')")))
  
  print(object_size(df))
  
  if (!is.null(keep)) {
    df <- df %>% select(any_of(keep))
  }
  if (add_filname) {
    df <- df %>% mutate(filename = paste(x,y))
  }
  print(object_size(df))
  print(paste(".",x,y))
  return(df)
}

# Merge two data frames
# Use filter_fct_diag and filter_fct_adm to extract specific diagnoses, procedures etc.
# Use lookup to standardize variable names
readlpr_dafr_dafr <- function(x,
                              y,
                              lookup=names_dict,
                              join_by="RECNUM",
                              filter_fct_diag = nofilter, # fct or list of fcts
                              filter_fct_adm = nofilter, # fct or list of fcts
                              keep = NULL,
                              add_filname=FALSE) {
  
  # Is data.frame?
  if (any(class(eval(parse(text=paste0(x)))) %in% c("tbl_df","tbl","data.frame"))) {
    xref <- x
  } else {
    stop(paste(x,"is not data.frame"))
  }
  
  # Is data.frame?
  if (any(class(eval(parse(text=paste0(y)))) %in% c("tbl_df","tbl","data.frame"))) {
    yref <- y
  } else {
    stop(paste(y,"is not data.frame"))
  }
  
  # Rename variables
  eval(parse(text=paste(xref,"<-",xref,"%>% rename(any_of(lookup))")))
  eval(parse(text=paste(yref,"<-",yref,"%>% rename(any_of(lookup))")))
  
  # Filter diagnoses
  eval(parse(text=paste(xref,"<-",xref,"%>% filter_fct_diag")))
  
  # Filter adm
  eval(parse(text=paste(yref,"<-",yref,"%>% filter_fct_adm")))
  
  # Join
  eval(parse(text=paste("df <- join(",yref,",",xref,",by=join_by,type='inner',match='all')")))
  
  if (!is.null(keep)) {
    df <- df %>% select(any_of(keep))
  }
  if (add_filname) {
    df <- df %>% mutate(filename = paste(x,y))
  }
  print(paste(".",x,y))
  return(df)
}

# Import sas file
# Use filter_fct to extract specific diagnoses, procedures etc.
# Use lookup to standardize variable names
# Use unique_id_df to filter population.
readsas <- function(x,
                    lookup=names_dict,
                    unique_id_df=NULL,
                    match_by="PNR",
                    path=rawg,
                    filter_fct=nofilter,
                    keep = NULL,
                    add_filname=FALSE) {
  
  # Is file?
  if (file.exists(paste0(path,x))) {
    df <- read_sas(paste0(path,x))
    xref <- "df"
  } else {
    stop(paste(x,"is not file"))
  }
  
  # Rename variables
  eval(parse(text=paste(xref,"<-",xref,"%>% rename(any_of(lookup))")))
  
  # Filter diagnoses
  eval(parse(text=paste(xref,"<-",xref,"%>% filter_fct")))
  
  # Join
  if (!is.null(unique_id_df)) {
    eval(parse(text=paste("df <- join(",xref,",",unique_id_df,",by=match_by,type='inner',match='all')")))
  }

  if (!is.null(keep)) {
    df <- df %>% select(any_of(keep))
  }
  if (add_filname) {
    df <- df %>% mutate(filename = x)
  }
  print(object_size(df))
  print(paste(".",x))
  return(df)
}

# Collapse overlapping or cohesive admissions
# int: maximum allowed time gap between cohesive admissions
collapse_time_n <- function(data,start,end,ID,int=0) {
  data %>% mutate(start_t={{start}},end_t={{end}}+int) %>% 
    arrange({{ID}},start_t,end_t) %>% 
    group_by({{ID}}) %>%
    mutate(counter = c(0,cumsum(as.numeric(lead(start_t)) >
                                  cummax(as.numeric(end_t)))[-n()])) %>%
    group_by({{ID}},counter) %>%
    summarize(start_t=min(start_t),end_t=max(end_t)-int) %>% ungroup()
}

# Filter functions
nofilter <- function(data,...) {
  data
}

# Filter procedures using list of procedure categories
filter_proc_codes <- function(data,filter_list,codename=PROCEDUREKODE,...) {
  for (row in 1:nrow(filter_list)) {
    data <- data %>%
      mutate(!!sym(filter_list$Name[row]) := grepl(filter_list$Regex[row],{{codename}}))
  }
  return(data %>% filter(if_any(filter_list$Name)))
}

# Filter primary and secondary diagnoses using list of diagnosis categories
filter_diags <- function(data,filter_list=diag_list,...) {
  data <- data %>% filter(grepl("^[ABCG]",DIAGNOSETYPE))
  for (row in 1:nrow(filter_list)) {
    data <- data %>%
      mutate(!!sym(filter_list$Name[row]) := grepl(filter_list$Regex[row],DIAGNOSEKODE))
  }
  return(data %>% filter(if_any(filter_list$Name)) %>% group_by(RECNUM) %>% summarise_at(vars(filter_list$Name),any) %>% ungroup())
}

# Filter heart failure diagnoses
filter_hf <- function(data,...) {
  data %>% filter(grepl("^[ABCG]",DIAGNOSETYPE)) %>% 
    filter(grepl("^DI(110|13[02]|42[069])",DIAGNOSEKODE)) %>%
    group_by(RECNUM) %>%
    summarise(hf=coalesce(TRUE)) %>%
    ungroup()
}

# Filter diagnoses for Chrons disease
filter_Crohns <- function(data,...) {
  data %>% filter(grepl("^[ABCG]",DIAGNOSETYPE)) %>%
    filter(grepl("^DK5[01]",DIAGNOSEKODE)) %>%
    group_by(RECNUM) %>%
    summarise(Crohns=coalesce(TRUE)) %>%
    ungroup()
}








