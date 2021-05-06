#######################################
# Initial set up
rm(list=ls())
options(stringsAsFactors=F)

library(readxl)
library(writexl)

output.dir="C:/Users/spounds/Desktop/Administrative/WebScrapePubs/2021-04-30/"

#######################################
# Read data

input.dir="C:/Users/spounds/OneDrive - St. Jude Children's Research Hospital/Publications_BIMS_AnalysisProjects/"
input.file=paste0(input.dir,"BIMS-PMID-matches-2021-04-12-for-dept.xlsx")

# read pair data
pair.data0=read_xlsx(input.file,sheet="pair.data")
pair.data0=as.data.frame(pair.data0)
valid.pairs=pair.data0[is.element(pair.data0$valid.match,1),1:2] # get PMID and BIMS of those marked 1 by the department

# read PubMed data
PM.data=read_xlsx(input.file,sheet="all.PubMed.data")
PM.data=as.data.frame(PM.data)
pm.pairs=PM.data[!is.na(PM.data$BIMS),c("PMID","BIMS")]

# read BIMS data
BIMS.data=read_xlsx(input.file,sheet="all.BIMS.data")
BIMS.data=as.data.frame(BIMS.data)
colnames(BIMS.data)[2]="BIMS"
BIMS.pairs=BIMS.data[!is.na(BIMS.data$PMID),c("PMID","BIMS")]

# read Jie's file
jie.file=paste0(input.dir,"BIMS-PMID-matches-2021-04-12-for-dept - JieHuanj.xlsx")
jie.data=read_xlsx(jie.file,sheet="pair.data")
jie.data=as.data.frame(jie.data)
jie.pairs=jie.data[is.element(jie.data$valid.match,1),1:2]

# read Stan's file
stan.file="H:/Stanley/BIMS-PubMed-Links/BIMS-PMID-matches-2021-04-12-for-dept.xlsx"
stan.pairs=read_xlsx(stan.file,sheet="pair.data")
stan.pairs=as.data.frame(stan.pairs)
stan.pairs=stan.pairs[is.element(stan.pairs$valid.match,1),1:2]

##################################
# combine all pairs data
all.pairs=rbind.data.frame(valid.pairs,
                           jie.pairs,
                           stan.pairs,
                           pm.pairs,
                           BIMS.pairs)

all.pairs$pair.string=paste0(all.pairs$BIMS,"_",all.pairs$PMID)

dup.pairs=which(duplicated(all.pairs$pair.string))
all.pairs=all.pairs[-dup.pairs,]


all.pair.data=merge(all.pairs,PM.data[,-grep("BIMS",colnames(PM.data))],
                    by="PMID")

all.pair.data=merge(BIMS.data[,-grep("PMID",colnames(BIMS.data))],all.pair.data,
                    by="BIMS")

########################################
# Summarize data

PM.BIMS.tbl=table(all.pair.data$BIMS,
                  all.pair.data$PMID)

length(unique(setdiff(PM.data$PMID,all.pair.data$PMID)))
length(unique(setdiff(BIMS.data$BIMS,all.pair.data$BIMS)))

##################################
# Find unannotated papers by faculty member

biostat.faculty=c("Cheng C","Kang G","Li Y",
                  "Lu Z","Mirzaei S","Mori M",
                  "Onar A","Onar-Thomas A",
                  "Pan H","Pounds S","Srivastava D",
                  "Srivastava K",
                  "Tang L")

unmatched.pubs=vector("list",length(biostat.faculty)+1)
names(unmatched.pubs)=c(biostat.faculty,"all.BIMS")

for (i in 1:length(biostat.faculty))
{
  faculty.rows=grep(biostat.faculty[i],PM.data$authors,ignore.case=T)
  match.rows=which(is.element(PM.data$PMID,all.pair.data$PMID))
  missed.rows=setdiff(faculty.rows,match.rows)
  unmatched.pubs[[i]]=as.data.frame(PM.data[missed.rows,])
  write_xlsx(PM.data[missed.rows,],
             paste0(output.dir,gsub(" ","-",biostat.faculty[i]),".xlsx"))
}

unmatched.pubs[["all.BIMS"]]=BIMS.data

write_xlsx(unmatched.pubs,
           paste0(output.dir,"Unmatched-Pubs-by-Faculty-Member.xlsx"))

write_xlsx(BIMS.data,paste0(output.dir,"BIMS-data.xlsx"))

#############################################
# Describe what we have now.

n.BIMS=nrow(BIMS.data)
n.pubs=nrow(PM.data)

n.matched.BIMS=length(unique(all.pair.data$BIMS))
n.matched.pubs=length(unique(all.pair.data$PMID))
n.matched.pairs=nrow(all.pair.data)

BIMS.tbl=table(all.pair.data$BIMS)

which(BIMS.tbl==13)
names(BIMS.tbl)[which(BIMS.tbl==13)]
