# <001> - include libraries
library(pdftools)
library(stringr)
library(dplyr)
# </001>
#---------------------------------------------------------------------
# <002> - include common funcs 

source("C:/DEV/R/R Scripts/CommonFunctions/loadComnFncs.R")
fncs <- c("getMaxVal.R")
loaded <- loadComnFncs(fncs)
# </002>
#---------------------------------------------------------------------

# <001> Start of script </001> 
ID <- 1 
DF_Final <- data.frame()
directory_path <- "C:/DEV/R/data/pastpapers"
# Check if the directory exists
dir_exists <- ifelse(dir.exists(directory_path),TRUE,FALSE) 

DF_MCQPs <- as.data.frame(read.csv("MCQPs.txt", sep = '/', header = TRUE ))
DF_MCQPs$matchstring <- tolower(paste0(DF_MCQPs$subject," ",DF_MCQPs$paper_no))

file_tlist1 <- sort(list.files(path = directory_path))
file_tlist2 <- tolower(unique(gsub(" memo", "", file_tlist1, ignore.case = TRUE)))

file_listmc <- c()
max_row <- getMaxVal(DF_MCQPs)

for (r in 1:max_row)
{
  file_listmc <- c(file_listmc, file_tlist2[which(grepl(DF_MCQPs$matchstring[r],file_tlist2))])
}


#--------------------------------------------------------------------------------
# Read the PDF files
#--------------------------------------------------------------------------------
QP_text  <- pdf_text(question_paper_path) %>% readr::read_lines() %>% str_trim() %>% tolower() %>% str_squish()
QP_text  <- QP_text[QP_text != ""]

Memo_text  <- pdf_text(memo_path) %>% readr::read_lines() %>% str_trim() %>% tolower() %>% str_squish()
Memo_text  <- Memo_text[Memo_text != ""]

regex_s_str <- "^1.1.1\\s"
regex_e_str <- "^1.2\\s"

qp_mc_s_inx <- which(grepl(regex_s_str,QP_text))
qp_mc_e_inx <- which(grepl(regex_e_str,QP_text))

if ((length(qp_mc_s_inx) != 1) || (length(qp_mc_e_inx) != 1))
  {
    message("error on start and end index for qp_mc_s_inx")
  } else 
  {
    QP_mc_text <- QP_text[qp_mc_s_inx:(qp_mc_e_inx-1)]
  }
#--------------------------------------------------------------------------------
#cleanup text 
#--------------------------------------------------------------------------------
remove_strings <- ("copyright|^nsc$|nss|^sce$|kopiereg|please turn|blaai om|dbe/|dbo/|doe/|totaal afdeling|groottotaal|sse")
extr_list      <- str_detect(QP_mc_text,remove_strings)
QP_mc_text    <- QP_mc_text[!extr_list]
QP_mc_text    <- gsub("\\(\\d+\\)", "", QP_mc_text)
QP_mc_text    <- gsub("\\(\\d+\\s*[Xx]\\s*\\d+\\)", "", QP_mc_text)

#--------------------------------------------------------------------------------
# Build question and answer per paper
#--------------------------------------------------------------------------------
a_z       <- getMaxVal(QP_mc_text)
a_a       <- 1 
DF_Final <- data.frame() 
for (a in a_a:a_z)
{
  if (str_detect(QP_mc_text[a],"^1\\.1\\.\\d+"))
  {
    message("found question start", QP_mc_text[a])
    message("index is", a)
    q_start <- a
    if (a > 1)
    {
      myrow <- nrow(DF_Final) + 1 
      opt_vec <- c(opt_vec,QP_mc_text[opt_start:(a-1)])
      DF_Final[myrow,1] <- paste(q_string, collapse = " ")
      DF_Final[myrow,2] <- paste(opt_vec, collapse = "#$#")
      qno <- str_split_i(DF_Final[myrow,1]," ",1) 
      qno_regex <- paste0(qno,"\\s")
      Ans_Inx <- which(grepl(qno_regex, Memo_text))
      message("question to find is ", qno, "regex ", qno_regex, "ans inx", Ans_Inx)
      if (is.na(Ans_Inx))
      {
        message("did not find question in memo")
        DF_Final[myrow,3] <- NA 
      } else 
      {
        memo_str <- Memo_text[Ans_Inx]
        Ans_s_pos   <- str_locate(memo_str, qno_regex)
        message("ans s pos", Ans_s_pos)
        if (anyNA(Ans_s_pos))
          {
            DF_Final[myrow,3] <- NA 
          } else 
          {
            Answer_string           <- substr(memo_str, start = Ans_s_pos[1,2]+1,stop = nchar(memo_str))
            Answer_string_trimmed   <- str_squish(Answer_string)
            Answer_letter           <- substr(Answer_string_trimmed,start= 1,stop = 1)
            Answer_index            <- grep(Answer_letter, c("a","b","c","d","e","f","g","h","i","j"))
            DF_Final[myrow,3] <- Answer_index
            
          }
      }
      
    }
  }
  if (str_detect(QP_mc_text[a],"^a\\s"))
  {
    q_string <- QP_mc_text[q_start:(a-1)]
    message("q string =", q_string)
    opt_start <- a
    opt_vec <- c()
  }
  if (str_detect(QP_mc_text[a],"^[b-f]\\s"))
  {
    opt_vec <- c(opt_vec,QP_mc_text[opt_start:(a-1)])
    opt_start <- a
  }
}
opt_vec <- c(opt_vec,QP_mc_text[opt_start:(a)])
myrow <- nrow(DF_Final) + 1 
DF_Final[myrow,1] <- paste(q_string, collapse = " ")
DF_Final[myrow,2] <- paste(opt_vec, collapse = "#$#")
qno <- str_split_i(DF_Final[myrow,1]," ",1) 
qno_regex <- paste0(qno,"\\s")
Ans_Inx <- which(grepl(qno_regex, Memo_text))
message("question to find is ", qno, "regex ", qno_regex, "ans inx", Ans_Inx)
if (is.na(Ans_Inx))
{
  message("did not find question in memo")
  DF_Final[myrow,3] <- NA 
} else 
{
  memo_str <- Memo_text[Ans_Inx]
  Ans_s_pos   <- str_locate(memo_str, qno_regex)
  message("ans s pos", Ans_s_pos)
  if (anyNA(Ans_s_pos))
  {
    DF_Final[myrow,3] <- NA 
  } else 
  {
    Answer_string           <- substr(memo_str, start = Ans_s_pos[1,2]+1,stop = nchar(memo_str))
    Answer_string_trimmed   <- str_squish(Answer_string)
    Answer_letter           <- substr(Answer_string_trimmed,start= 1,stop = 1)
    Answer_index            <- grep(Answer_letter, c("a","b","c","d","e","f","g","h","i","j"))
    DF_Final[myrow,3]       <- Answer_index
  }
}
write.csv(DF_Final,"QnA_DF.csv", row.names = FALSE)




#qno <- str_split_i(QP_mc_text[a]," ",1) 
#qno_regex <- paste0(qno,"\\s")
#Ans_Inx <- which(grepl(qno_regex, Memo_text))
#if (is.na(Ans_Inx))
#{
#  message("did not find question in memo")
#  DF_Final[myrow,3] <- NA 
#} else 
#{
#  memo_str <- Memo_text[Ans_Inx]
#  Ans_s_pos   <- str_locate(memo_str, qno_regex)
#  message("ans s pos", Ans_s_pos)
#  if (anyNA(Ans_s_pos))
#  {
#    DF_Final[myrow,3] <- NA 
#  } else 
#  {
#    Answer_string           <- substr(memo_str, start = Ans_s_pos[1,2]+1,stop = nchar(memo_str))
#    Answer_string_trimmed   <- str_squish(Answer_string)
#    Answer_letter           <- substr(Answer_string_trimmed,start= 1,stop = 1)
#    Answer_index            <- grep(Answer_letter, c("a","b","c","d","e","f","g","h","i","j"))
#    message("answer index", Answer_index)
#    if (a > 1)
#    {
#      DF_Final[myrow,3] <- Answer_index
#    }
#  }
#}
#message ("q no = ", qno, "ans inx", Ans_Inx)
#memo_string <- Memo_text[Ans_Inx]
