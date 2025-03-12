

diag_list <- read_code_list("diag_regex.xlsx")
atc_list <- read_code_list("atc_regex.xlsx")
proc_list <- read_code_list("proc_regex.xlsx")
nmi_atc_list <- read_code_list("nmi_atc_regex.xlsx")
nmi_diag_list <- read_code_list("nmi_diag_regex.xlsx")

diag_wdict     <- as.list(t(diag_list %>% select(Weight)));     names(diag_wdict)     <- diag_list$Name
atc_wdict      <- as.list(t(atc_list %>% select(Weight)));      names(atc_wdict)      <- atc_list$Name
proc_wdict     <- as.list(t(proc_list %>% select(Weight)));     names(proc_wdict)     <- proc_list$Name
nmi_atc_wdict  <- as.list(t(nmi_atc_list %>% select(Weight)));  names(nmi_atc_wdict)  <- nmi_atc_list$Name
nmi_diag_wdict <- as.list(t(nmi_diag_list %>% select(Weight))); names(nmi_diag_wdict) <- nmi_diag_list$Name

diag_wdict[['astma']]

names_keep <- c(
  "PNR",
  "DATO_START",
  diag_list$Name,
  atc_list$Name,
  proc_list$Name,
  nmi_atc_list$Name,
  nmi_diag_list$Name
)

# Get ADM
names_dict_adm <- c(
  PNR = "PNR",
  RECNUM = "RECNUM",
  RECNUM = "DW_EK_KONTAKT",
  DATO_START_ADM = "DATO_START",
  DATO_START_ADM = "D_INDDTO"
)
names_keep_adm <- c(
  "PNR",
  "DATO_START_ADM",
  "RECNUM",
  "DW_EK_FORLOEB"
)
file_list_adm <- list.files(path=rawg,pattern="(lpr_adm20(1[2-9]|2[0-3])|lpr_f_(kontakter|forloeb)2022).sas7bdat")
lpr_list_adm <- map(file_list_adm,
                    readsas,
                    lookup=names_dict_adm,
                    path=rawg,
                    keep=names_keep_adm)

# Get DIAG
names_keep_diag <- c(
  "PNR",
  "DATO_START_ADM",
  diag_list$Name,
  nmi_diag_list$Name
)
lpr_list_adm_str <- deparse_list(lpr_list_adm)
file_list_diag <- list.files(path=rawg,pattern="(lpr_diag20(1[2-9]|2[0-3])|lpr_f_diagnoser2022).sas7bdat")
lpr_diag <- map2(file_list_diag,
                 lpr_list_adm_str[c(1:8,10)],
                 readlpr_file_dafr,
                 path=rawg,
                 filter_fct_diag=function(x){filter_diags(x,filter_list=rbind(diag_list,nmi_diag_list))},
                 keep=names_keep_diag,
                 add_filname=TRUE) %>% bind_rows






# Read procedures:
names_keep_proc <- c(
  "PNR",
  "DATO_START_ADM",
  "DATO_START",
  proc_list$Name,
)
file_list_proc <- list.files(path=rawg,pattern="lpr_sksube20(1[2-9]|2[0-3]).sas7bdat")
file_list_proc <- c(file_list_proc,
                    list.files(path=rawg,pattern="lpr_f_procedurer_andre2022.sas7bdat"))

lpr_proc <- map2(file_list_proc,
                 lpr_list_adm_str[c(1:8,10)],
                 readlpr_file_dafr,
                 path=rawg,
                 filter_fct_diag=function(x){filter_proc_codes(x,filter_list=rbind(proc_list))},
                 keep=names_keep_proc,
                 add_filname=TRUE) %>% bind_rows

lpr_proc <- rbind(lpr_proc,
                  map2(file_list_proc[9],
                       lpr_list_adm_str[9],
                       readlpr_file_dafr,
                       path=rawg,
                       join_by="DW_EK_FORLOEB",
                       filter_fct_diag=function(x){filter_proc_codes(x,filter_list=rbind(proc_list))},
                       keep=names_keep_proc,
                       add_filname=TRUE))



# Read LMDB
file_list <- list.files(path=raw_g,pattern="lmdb.*sas7bdat")
file_list <- file_list[file_list!="lmdb202212.sas7bdat"]
lmdb1 <- map_df(file_list,readsas,unique_id_df=pop_id_index,filter_fct = filter_meds)
file_list <- list.files(path=paste0(raw_g,"/Version\ 4/"),pattern="lmdb.*sas7bdat")
lmdb2 <- map_df(file_list,readsas,unique_id_df=pop_id_index,filter_fct = filter_meds,path=paste0(raw_g,"/Version\ 4/"))

lmdb_copy <- rbind(lmdb1,lmdb2)


# Read immigration/emigration data
vnds <- read_sas(paste0(raw_g,'vnds2022.sas7bdat'))
table(is.na(vnds$HAEND_DATO))
table(is.na(vnds$INDUD_KODE))
vnds <- vnds %>% filter(PNR != "") %>%
  mutate(exit = case_when(INDUD_KODE == "U" ~ HAEND_DATO,.default=NA),
         enter = case_when(INDUD_KODE == "I" ~ HAEND_DATO,.default=NA))

vnds <- vnds %>% arrange(PNR,HAEND_DATO) %>% group_by(PNR) %>%
  summarise(enter=first(enter),
            exit=last(exit))





