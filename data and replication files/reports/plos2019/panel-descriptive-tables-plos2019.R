# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# R2D2
# Panel descirptive manuscript analysis
# Amy Finnegan, amy.finnegan@duke.edu
# .............................................................................
# This is the file that creates the .tex tables for the
# panel descriptive manuscript published in PLOS One
# 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Table 1. descriptives =======================================================


# parent
t <- xtable(descTable[1:9,])
f <- paste("public/reports/plos2019/output/tables/tbl-descP", "tex",
           sep=".")
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      hline.after=NULL,
      type="latex",
      file=f,
      sanitize.text.function=identity
)

# child
t <- xtable(descTable[10:12,])
f <- paste("public/reports/plos2019/output/tables/tbl-descC", "tex",
           sep=".")
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      hline.after=NULL,
      type="latex",
      file=f,
      sanitize.text.function=identity
)

# Table 2. follow through long ================================================
t <- xtable(byIntendLong)
f <- paste("public/reports/plos2019/output/tables/tbl-followThroughLong", "tex",
           sep=".")
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      hline.after=NULL,
      type="latex",
      file=f,
      sanitize.text.function=identity
)

# Table B1. cgtype ============================================================

t <- xtable(rel)
f <- paste("public/reports/plos2019/output/tables/tbl-cgtype", "tex",
           sep=".")
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      hline.after=NULL,
      type="latex",
      file=f,
      sanitize.text.function=identity
)

# Table B2. disclosureStatus ==================================================

# knows, etc.
t <- xtable(npctDisc)
f <- paste("public/reports/plos2019/output/tables/tbl-status", "tex",
           sep=".")
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      hline.after=NULL,
      type="latex",
      file=f,
      sanitize.text.function=identity
)

#PNF
t <- xtable(npctDiscCat)
f <- paste("public/reports/plos2019/output/tables/tbl-statusPNF", "tex",
           sep=".")
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      hline.after=NULL,
      type="latex",
      file=f,
      sanitize.text.function=identity
)




# Table B3. worries ===========================================================
t <- xtable(risks)
f <- paste("public/reports/plos2019/output/tables/tbl-worries", "tex",
           sep=".")
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      hline.after=NULL,
      type="latex",
      file=f,
      sanitize.text.function=identity
)


# Table C1. lit table =========================================================

citeKeys <-
  citeKeys %>%
  select(-nameRD)
t <- xtable(citeKeys)
f <- paste("public/reports/plos2019/output/tables/tbl-lit", "tex",
           sep=".")
print(t,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames=FALSE,
      hline.after=NULL,
      type="latex",
      file=f,
      sanitize.text.function=identity
)

