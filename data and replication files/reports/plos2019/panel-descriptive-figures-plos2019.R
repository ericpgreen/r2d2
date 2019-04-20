# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# R2D2
# Panel descirptive manuscript analysis
# Amy Finnegan, amy.finnegan@duke.edu
# .............................................................................
# This is the file that creates the figures
# for the panel descriptive manuscript published in PLOS One
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



# alluvium color functions ===================================================

# function to change colors of alluvium by column
alluviumColor <- function(background=color1,
                          facet1,
                          highlight1=color2,
                          facet2,
                          highlight2=color3,
                          stratum,
                          pickCol,
                          data) {
  lrow <- nrow(data)
  subset <- data %>% select_(., .dots=pickCol)
  lcol <- subset %>% length(.)
  cols <- rep(background, lrow * lcol)
  if (facet1 != FALSE) {
    for (f in facet1) {
      cols[which(data[,stratum]==f)] <- highlight1
    }
  }
  if (facet2 != FALSE) {
    for (f in facet2) {
      cols[which(data[,stratum]==f)] <- highlight2
    }
  }
  return(cols)
}



# #for example:
# alluviumColor(background="gray",
#               stratum="p.disclosureStatus.r3",
#               facet1="HIV Only",
#               highlight1="yellow",
#               facet2="Full",
#               highlight2="green",
#               data=ageNPF)

# function to change stratum colors
stratumColor <- function(data, pickCol) { # for a data set
  subset <- data %>% select_(., .dots=pickCol)
  lcol <- length(subset) # number of columns
  s <- list() # empty list
  for (i in 1:lcol) {
    s[[i]] <- as.character(unique(subset[,i])) # grab unique values and put in list
  }
  s <- unlist(s) # character vector
  s <-
    # if gray
    #ifelse(s=="< 12 yrs", gray.colors(7)[7],
    #ifelse(s=="12+ yrs", gray.colors(7)[7],

    #if dghi
    ifelse(s=="< 12 yrs", orange,
    ifelse(s=="12+ yrs", orange,

     ifelse(s=="no disclosure", gray.colors(7)[7],
     ifelse(s=="partial, knows name only", gray.colors(7)[4],
     ifelse(s=="partial, knows name and how infected", gray.colors(7)[4],
     ifelse(s=="partial, knows name and can spread", gray.colors(7)[4],
     ifelse(s=="full disclosure", gray.colors(7)[1],

      # if gray
      # ifelse(s=="Full", gray.colors(7)[1],
      # ifelse(s=="HIV &\nHow Infected", gray.colors(7)[4],
      # ifelse(s=="HIV & Can Spread", gray.colors(7)[4],
      # ifelse(s=="HIV Only", gray.colors(7)[4],
      # ifelse(s=="No Disclosure", gray.colors(7)[7],

     # if dghi colors
     ifelse(s=="Full", green,
     ifelse(s=="HIV &\nHow Infected", blue,
     ifelse(s=="HIV & Can Spread", blue,
     ifelse(s=="HIV Only", blue,
     ifelse(s=="No Disclosure", orange,

     ifelse(s=="Knows HIV", colfunc(7)[5],
     ifelse(s=="Knows Illness Only", colfunc(7)[3],
     ifelse(s=="Knows Nothing", colfunc(7)[1],

     ifelse(s=="Knows", colfunc(7)[5],
     ifelse(s=="Doesn't Know", colfunc(7)[1],

     "black"))))))))))))))))) # if not in list
  return(s)
}

# for example:
#stratumColor(ageNPF)


# Figure B.3 - main flow figures ==============================================

# order of alluvium
discLevels = c("no disclosure",
               "partial, knows name only",
               "partial, knows name and how infected",
               "partial, knows name and can spread",
               "full disclosure")
NPF <-
  datW %>%
  filter(p.knowsHIV.r1==0) %>%
  select(p.disclosureStatus.r1, p.disclosureStatus.r2, p.disclosureStatus.r3) %>%
  mutate(p.disclosureStatus.r1=ifelse(p.disclosureStatus.r1=="partial, not named HIV", "no disclosure", p.disclosureStatus.r1),
         p.disclosureStatus.r2=ifelse(p.disclosureStatus.r2=="partial, not named HIV", "no disclosure", p.disclosureStatus.r2),
         p.disclosureStatus.r3=ifelse(p.disclosureStatus.r3=="partial, not named HIV", "no disclosure", p.disclosureStatus.r3)) %>%
  mutate(p.disclosureStatus.r1=factor(p.disclosureStatus.r1,
                                      levels=discLevels,
                                      ordered=T)) %>%
  mutate(p.disclosureStatus.r2=factor(p.disclosureStatus.r2,
                                      levels=discLevels,
                                      ordered=T)) %>%
  mutate(p.disclosureStatus.r3=factor(p.disclosureStatus.r3,
                                      levels=discLevels,
                                      ordered=T)) %>%
  count_(., names(.)) %>%
  data.frame(.) %>%
  filter(complete.cases(.)) %>%
  mutate(pct=as.numeric(rd0(n/sum(n)*100))) %>%
  data.frame(.)

for (v in c("p.disclosureStatus.r1", "p.disclosureStatus.r2", "p.disclosureStatus.r3")) {
  NPF[,v] <- ifelse(NPF[,v]=="no disclosure", "No Disclosure",
             ifelse(NPF[,v]=="partial, knows name only", "HIV Only",
             ifelse(NPF[,v]=="partial, knows name and how infected", "HIV &\nHow Infected",
             ifelse(NPF[,v]=="partial, knows name and can spread", "HIV & Can Spread",
             ifelse(NPF[,v]=="full disclosure", "Full", "WTF")))))
}

NPF$p.disclosureStatus.r2 <- factor(NPF$p.disclosureStatus.r2,
                                    levels=c("No Disclosure",
                                             "HIV Only",
                                             "HIV &\nHow Infected",
                                             "HIV & Can Spread",
                                             "Full",
                                             "WTF"))
NPF$p.disclosureStatus.r3 <- factor(NPF$p.disclosureStatus.r3,
                                    levels=c("No Disclosure",
                                             "HIV Only",
                                             "HIV &\nHow Infected",
                                             "HIV & Can Spread",
                                             "Full",
                                             "WTF"))

pickCol <- c("p.disclosureStatus.r1", "p.disclosureStatus.r2", "p.disclosureStatus.r3")
NPFplot <-
  ggplot(NPF,
         aes(weight=pct, axis1 = p.disclosureStatus.r1, axis2 = p.disclosureStatus.r2,
             axis3 = p.disclosureStatus.r3)) +
  geom_alluvium(aes(),fill=alluviumColor(background=orange,
                                         stratum="p.disclosureStatus.r3",
                                         facet1="Full",
                                         highlight1=green,
                                         facet2=c("HIV &\nHow Infected",
                                                  "HIV Only",
                                                  "HIV & Can Spread"),
                                         highlight2=blue,
                                         pickCol=pickCol,
                                         data=NPF),
                color="white") + # color controls flow color outline
  geom_stratum(color = "black", fill=stratumColor(NPF,
                                                  pickCol=pickCol)) + # color controls box color outline
  geom_text(stat="stratum", label.strata=TRUE, size=3) + #, vjust="inward", hjust="inward") +
  scale_x_continuous(breaks = 1:3, labels = c("Baseline", "6 months", "12 months")) +
  theme_minimal() +
  theme(axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=12)) +
  ylab("Percent (%)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
cairo_pdf(paste("public/reports/plos2019/output/figures/NPFDGHI", "pdf", sep="."),
          width=9, height=9)
print(NPFplot)
dev.off()


# Figure 2 - cumulative % who know each thing by age ==========================

zeroDat <- data.frame(variable=c(rep("ageLearnedHIV", min(datW$ageLearnedHIV, na.rm=T)),
                                 rep("ageLearnedHowInf", min(datW$ageLearnedHowInf, na.rm=T)),
                                 rep("ageLearnedCanSpread", min(datW$ageLearnedCanSpread, na.rm=T)),
                                 rep("ageLearnedChronic", min(datW$ageLearnedChronic, na.rm=T)),
                                 rep("ageFullDisclosure.r1", min(datW$ageFullDisclosure.r1, na.rm=T))),
                      value=c(seq(0, min(datW$ageLearnedHIV, na.rm=T)-1, 1),
                              seq(0, min(datW$ageLearnedHowInf, na.rm=T)-1, 1),
                              seq(0, min(datW$ageLearnedCanSpread, na.rm=T)-1, 1),
                              seq(0, min(datW$ageLearnedChronic, na.rm=T)-1, 1),
                              seq(0, min(datW$ageFullDisclosure.r1, na.rm=T)-1, 1)),
                      count=0,
                      all=372,
                      csum=0,
                      pct=0,
                      label=NA)


kHIVdat <-
  datW %>%
  mutate(ageLearnedHIV=ifelse(waveLearnedHIV!="r1", NA, ageLearnedHIV)) %>%
  mutate(ageLearnedHowInf=ifelse(waveLearnedHowInf!="r1", NA, ageLearnedHowInf)) %>%
  mutate(ageLearnedCanSpread=ifelse(waveLearnedCanSpread!="r1", NA, ageLearnedCanSpread)) %>%
  mutate(ageLearnedChronic=ifelse(waveLearnedChronic!="r1", NA, ageLearnedChronic)) %>%
  select(pid, ageLearnedHIV,
         ageLearnedHowInf,
         ageLearnedCanSpread,
         ageLearnedChronic,
         ageFullDisclosure.r1) %>%
  melt(., id="pid") %>%
  #filter(!is.na(value)) %>%
  group_by(variable, value) %>%
  dplyr::summarise(count=n()) %>%
  group_by(variable) %>%
  mutate(all=sum(count),
         count=as.numeric(count),
         value=as.numeric(value)) %>%
  arrange(variable, value) %>%
  group_by(variable) %>%
  mutate(csum=cumsum(count),
         pct=(csum/all)*100) %>%
  filter(!is.na(value)) %>%
  filter(variable!="c.age.r1") %>%
  mutate(label=ifelse(variable=="ageLearnedChronic", "Chronic Condition",
               ifelse(variable=="ageLearnedHIV", "HIV Status",
               ifelse(variable=="ageLearnedCanSpread", "Can Spread",
               ifelse(variable=="ageLearnedHowInf", "How Infected",
               ifelse(variable=="ageFullDisclosure.r1", "Full Disclosure", "WTF")))))) %>%
  data.frame(.) %>%
  rbind(., zeroDat) %>%
  mutate(HIV=ifelse(variable=="ageLearnedHIV", "Y", "N"))

kHIV <-
  ggplot(kHIVdat, aes(x=as.factor(value), y=as.numeric(pct),
                      color=variable, group=variable, label=label, linetype=variable)) +
  geom_line(aes(size=HIV)) +
  scale_size_manual(values=c(1,2)) +
  ylim(0,100) +
  labs(x="Age", y="Percent (%)") +
  theme_minimal() +
  geom_hline(yintercept=50, linetype="dotted") +
  theme(legend.title=element_blank()) +
  geom_text(data=subset(kHIVdat, value==15), vjust=c(rep(-1, 4), 4), hjust=1) +
  scale_color_manual(values=c("black", rep("black", 4))) +
  scale_linetype_manual(values=c("solid", "solid", "solid",
                                 "dashed", "solid"), name="") +
  guides(color=FALSE) +
  guides(linetype=FALSE) +
  guides(size=FALSE) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  theme(legend.position = "bottom")

cairo_pdf(paste("public/reports/plos2019/output/figures/kHIV", "pdf", sep="."))
print(kHIV)
dev.off()

CairoPNG(paste("public/reports/plos2019/output/figures/kHIV", "png", sep="."))
print(kHIV)
dev.off()


tiff(paste("public/reports/plos2019/output/figures/kHIV", "tiff", sep="."),
     width=5, height=5, units="in", res=300)
print(kHIV)
dev.off()


# Figure 4 - reported pediatric HIV disclosure in literature ==================
# two panels
# panel 1: y=study, x=pct disclosed, sort by disclosed, highest on top
# panel 2: y=study, sorted by pct disclosed, x=age range of study

dpct <- read.xlsx("public/resources/disclosure_pct.xlsx", sheetName="Sheet1")

datPct <-
  dpct %>%
  filter(grepl("Atwiine", study)==FALSE) %>% # removed b/c this is "full disclosure"
  filter(grepl("Kajubi", study)==FALSE) %>% # removed b/c you can't tell if caregivers disclosed "HIV"
  mutate(estimate=ifelse(is.na(estimate), "", as.character(estimate))) %>%
  mutate(nameRD=paste0(study, ", ", year, estimate, " (", country, ")")) %>%
  mutate(name=paste0(study, ", ", year, " (", country, ")")) %>%
  arrange(pctDisclosed) %>%
  #mutate(id=seq_along(dpct$country)) %>%
  filter(!is.na(minAge)) %>%
  mutate(ours=ifelse(grepl("Finnegan", name), "Our Study", "not")) %>%
  mutate(name=ifelse(grepl("Finnegan", name), "THIS STUDY (Zimbabwe)", name)) %>%
  mutate(nameRD=ifelse(grepl("Finnegan", nameRD), "THIS STUDY (Zimbabwe)", nameRD)) %>%
  mutate(psam=ifelse(!is.na(psam), 1, 0)) %>%
  filter(!is.na(sampleSize))

# counts by sample type
pct.ns <-
  datPct %>%
  filter(reportedDerived=="R") %>%
  summarise(cg.ivw=sum(Caregiver.only.interviewed, na.rm=T),
            both.ivw=sum(Caregiver.child.interviewed, na.rm=T),
            mr.ivw=sum(Extracted.from.medical.records, na.rm=T),
            cg.r=sum(Caregiver.reported.disclosure, na.rm=T),
            child.r=sum(Child.reported.disclosure, na.rm=T),
            mr.r=sum(Extracted.from.medical.records, na.rm=T),
            unclear.r=sum(Unclear.reported.disclosure, na.rm=T))


cg.ivw.n <- pct.ns$cg.ivw
both.ivw.n <- pct.ns$both.ivw
mr.ivw.n <- pct.ns$mr.ivw

caregiver.r.n <- pct.ns$cg.r-1
child.r.n <- pct.ns$child.r
unclear.r.n <- pct.ns$unclear.r
mr.r.n <- pct.ns$mr.r

# create cite-keys
citeKeys <-
  datPct %>%
  mutate(study2=paste0(gsub("&", "\\\\&", study), " (", year, estimate, ")")) %>%
  mutate(study=str_split(as.character(study), ",", simplify=TRUE)[,1]) %>%
  mutate(study=str_split(as.character(study), " &", simplify=TRUE)[,1]) %>%
  mutate(key=paste0(str_to_lower(study), ":", year)) %>%
  #mutate(key=paste0(study2, " \\citeappendix{", key, "lf", "}")) %>%
  mutate(key=paste0(study2, " \\cite{", key, "}")) %>%
  # mutate(key=paste0("\\citeappendix{", key, "lf", "}",
  #                   " ",
  #                   study2)) %>% # multibib new appendix
  #mutate(nameRD=ifelse(reportedDerived=="D", paste0("\\textit{", nameRD, "}"), nameRD)) %>%
  mutate(minAge=rd0(minAge),
         maxAge=rd0(maxAge)) %>%
  mutate(minAge=ifelse(minAge==0, "<1", minAge)) %>%
  mutate(sampleSize=rd0(as.numeric(sampleSize))) %>%
  mutate(reportedDerived=ifelse(reportedDerived=="R", "reported",
                                ifelse(reportedDerived=="D", "derived", "reported"))) %>%
  #mutate(pctDisclosed=rd1(pctDisclosed)) %>%
  mutate(key=ifelse(grepl("finnegan", key), "THIS STUDY", key)) %>%
  arrange(., desc(pctDisclosed))

litExcel <-
  citeKeys %>%
  select(study2, country, minAge, maxAge, pctDisclosed, sampleSize, reportedDerived)

citeKeys <-
  citeKeys %>%
  select(key, nameRD, country, minAge, maxAge, pctDisclosed, sampleSize, reportedDerived)

# study x pct disclosed - reported only
studyDisclosed <-
  datPct %>%
  filter(reportedDerived=="R") %>%
  select(pctDisclosed, name, ours, sampleSize, psam) %>%
  ggplot(., aes(x=pctDisclosed, y=reorder(as.character(name), pctDisclosed),
                color=as.factor(psam), shape=as.factor(psam), label=sampleSize)) +
  geom_point() +
  geom_text(size=2, hjust=1.5) +
  xlim(-5,100) +
  labs(x="Percent Disclosed",
       y="") +
  theme_bw() +
  scale_color_manual(values=c(gray.colors(7)[4],
                              "black")) +
  scale_shape_manual("", values=c(1,19),
                     labels=c("Convenience Sample",
                              "Probability Sample")) +
  theme(legend.position = c(0.25,0.9)) +
  theme(panel.grid=element_blank()) +
  guides(color=FALSE) +
  #guides(shape=FALSE) +
  theme(axis.text.y=element_text(size=6)) +
  theme(legend.background=element_rect(fill="transparent")) +
  theme(legend.text=element_text(size=6))
cairo_pdf(paste("public/reports/plos2019/output/figures/studyDisclosed", "pdf", sep="."),
          height=12,
          width=6)
print(studyDisclosed)
dev.off()

# study x age range - reported only
studyAgeRange <-
  datPct %>%
  select(minAge, maxAge, name, pctDisclosed, ours, psam, reportedDerived) %>%
  filter(reportedDerived=="R") %>%
  melt(., id=c("name", "pctDisclosed", "ours", "psam", "reportedDerived")) %>%
  filter(!is.na(value)) %>%
  ggplot(., aes(x=value, y=reorder(as.character(name), pctDisclosed),
                group=name, shape=as.factor(psam), color=as.factor(psam))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c(gray.colors(7)[4],
                              "black")) +
  scale_shape_manual(values=c(1,19)) +
  guides(shape=FALSE) +
  guides(color=FALSE) +
  labs(x="Study Age Range",
       y="") +
  xlim(0,20) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
cairo_pdf(paste("public/reports/plos2019/output/figures/studyAgeRange", "pdf", sep="."),
          height=12,
          width=2)
print(studyAgeRange)
dev.off()

pdf("public/reports/plos2019/output/figures/litFig.pdf")
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(studyDisclosed, vp=vplayout(1,1:2))
print(studyAgeRange, vp=vplayout(1,3))
dev.off()

tiff("public/reports/plos2019/output/figures/litFig.tiff",
     height=12,
     width=8,
     units="in",
     res=300)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(studyDisclosed, vp=vplayout(1,1:2))
print(studyAgeRange, vp=vplayout(1,3))
dev.off()

png("public/reports/plos2019/output/figures/litFig.png")
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(studyDisclosed, vp=vplayout(1,1:2))
print(studyAgeRange, vp=vplayout(1,3))
dev.off()

# Figure C.1 - all reported and derived estimates =============================

# study x pct disclosed - reported + derived
studyDisclosedRD <-
  datPct %>%
  select(pctDisclosed, nameRD, ours, sampleSize, psam) %>%
  ggplot(., aes(x=pctDisclosed, y=reorder(as.character(nameRD), pctDisclosed),
                color=as.factor(psam), shape=as.factor(psam), label=sampleSize)) +
  geom_point() +
  geom_text(size=2, hjust=1.5) +
  xlim(-5,100) +
  labs(x="Percent Disclosed",
       y="") +
  theme_bw() +
  scale_color_manual(values=c(gray.colors(7)[4],
                              "black")) +
  scale_shape_manual("", values=c(1,19),
                     labels=c("Convenience Sample",
                              "Probability Sample")) +
  theme(legend.position = c(0.25,0.9)) +
  theme(panel.grid=element_blank()) +
  guides(color=FALSE) +
  #guides(shape=FALSE) +
  theme(axis.text.y=element_text(size=6)) +
  theme(legend.background=element_rect(fill="transparent")) +
  theme(legend.text=element_text(size=6))
cairo_pdf(paste("public/reports/plos2019/output/figures/studyDisclosedRD", "pdf", sep="."),
          height=12,
          width=6)
print(studyDisclosedRD)
dev.off()

# study x age range - reported + derived
studyAgeRangeRD <-
  datPct %>%
  select(minAge, maxAge, nameRD, pctDisclosed, ours, psam) %>%
  melt(., id=c("nameRD", "pctDisclosed", "ours", "psam")) %>%
  filter(!is.na(value)) %>%
  ggplot(., aes(x=value, y=reorder(as.character(nameRD), pctDisclosed),
                group=nameRD, shape=as.factor(psam), color=as.factor(psam))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c(gray.colors(7)[4],
                              "black")) +
  scale_shape_manual(values=c(1,19)) +
  guides(shape=FALSE) +
  guides(color=FALSE) +
  labs(x="Study Age Range",
       y="") +
  xlim(0,20) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
cairo_pdf(paste("public/reports/plos2019/output/figures/studyAgeRangeRD", "pdf", sep="."),
          height=12,
          width=2)
print(studyAgeRangeRD)
dev.off()

pdf("public/reports/plos2019/output/figures/litFigRD.pdf")
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(studyDisclosedRD, vp=vplayout(1,1:2))
print(studyAgeRangeRD, vp=vplayout(1,3))
dev.off()

# Figure B.2 - prob of knowing each - baseline ================================

baseStat <-
  datW %>%
  select(p.knowsChronic.r1, p.knowsHIV.r1, p.knowsHowInf.r1, p.knowsCanSpread.r1, p.fullDisclosure.r1, c.age.r1) %>%
  melt(., id="c.age.r1") %>%
  group_by(c.age.r1, variable) %>%
  mutate(value=ifelse(is.na(value), 0, value)) %>%
  summarise(n=sum(!is.na(value)),
            m=rd0(mean(value, na.rm=T)*100)) %>%
  mutate(variable=ifelse(variable=="p.knowsChronic.r1", "Chronic Condition",
                  ifelse(variable=="p.knowsHIV.r1", "Knows HIV Status",
                  ifelse(variable=="p.knowsHowInf.r1", "HIV & How Infected",
                  ifelse(variable=="p.knowsCanSpread.r1", "HIV & Can Spread",
                  ifelse(variable=="p.fullDisclosure.r1", "Full Disclosure", "WTF")))))) %>%
  mutate(variable=factor(variable,
                         levels=c("Chronic Condition",
                                  "Knows HIV Status",
                                  "HIV & How Infected",
                                  "HIV & Can Spread",
                                  "Full Disclosure",
                                  "WTF"))) %>%
  filter(variable != "Chronic Condition") %>%
  droplevels(.) %>%
  ggplot(., aes(x=as.factor(c.age.r1), y=as.numeric(m), label=m)) +
  geom_bar(stat="identity") +
  geom_text(color="white", hjust=1.5) +
  facet_wrap(~variable, nrow=1) +
  ylim(0,100) +
  labs(x="Age",
       y="Percent (%)") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y=element_text(size=14)) +
  theme(axis.title=element_text(size=14)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text=element_text(size=12))
cairo_pdf(paste("public/reports/plos2019/output/figures/baseStat", "pdf", sep="."),
          width=14, height=7)
print(baseStat)
dev.off()

# create data frame for incidence plots ---
pch <- datW$pid[datW$p.knowsHIV.r1==0]

datSurv <-
  datW %>%
  filter(pid %in% pch) %>%
  select(doesKnow19When.r2, doesKnow19When.r3,
         doesKnow23When.r2, doesKnow23When.r3,
         doesKnow27When.r2, doesKnow27When.r3,
         p.fullDisclosure.r3,
         c.age.r1) %>%
  mutate(learnedFull=
           case_when(
             p.fullDisclosure.r3==1 ~ "1",
             p.fullDisclosure.r3==0 ~ "0",
             TRUE ~ "0"
           )) %>%
  mutate(learnedFull=as.numeric(learnedFull)) %>%
  mutate(timeLearnedHIV= # HIV ---
         case_when(
           doesKnow19When.r2=="before" ~ "0",
           doesKnow19When.r2=="june" ~ "0",
           doesKnow19When.r2=="july" ~ "1",
           doesKnow19When.r2=="august" ~ "2",
           doesKnow19When.r2=="september" ~ "3",
           doesKnow19When.r2=="october" ~ "4",
           doesKnow19When.r2=="november" ~ "5",
           doesKnow19When.r2=="december" ~ "6",
           doesKnow19When.r2=="january" ~ "7",

           doesKnow19When.r3=="before" ~ "8",
           doesKnow19When.r3=="february" ~ "8",
           doesKnow19When.r3=="march" ~ "9",
           doesKnow19When.r3=="april" ~ "10",
           doesKnow19When.r3=="may" ~ "11",
           doesKnow19When.r3=="june" ~ "12",
           doesKnow19When.r3=="july" ~ "13",

           TRUE ~ "13"
         ),
         timeLearnedHIV=as.numeric(timeLearnedHIV)) %>%
  mutate(learnedHIV=
           case_when(
             is.na(doesKnow19When.r2) & is.na(doesKnow19When.r3) ~ "0",
             TRUE ~ "1"
           ),
         learnedHIV=as.numeric(learnedHIV)) %>%
  mutate(timeLearnedHowInf= # how infected ---
         case_when(
           doesKnow23When.r2=="before" ~ "0",
           doesKnow23When.r2=="june" ~ "0",
           doesKnow23When.r2=="july" ~ "1",
           doesKnow23When.r2=="august" ~ "2",
           doesKnow23When.r2=="september" ~ "3",
           doesKnow23When.r2=="october" ~ "4",
           doesKnow23When.r2=="november" ~ "5",
           doesKnow23When.r2=="december" ~ "6",
           doesKnow23When.r2=="january" ~ "7",

           doesKnow23When.r3=="before" ~ "8",
           doesKnow23When.r3=="february" ~ "8",
           doesKnow23When.r3=="march" ~ "9",
           doesKnow23When.r3=="april" ~ "10",
           doesKnow23When.r3=="may" ~ "11",
           doesKnow23When.r3=="june" ~ "12",
           doesKnow23When.r3=="july" ~ "13",

           TRUE ~ "13"
         ),
         timeLearnedHowInf=as.numeric(timeLearnedHowInf)) %>%
  mutate(learnedHowInf=
           case_when(
             is.na(doesKnow23When.r2) & is.na(doesKnow23When.r3) ~ "0",
             TRUE ~ "1"
           ),
         learnedHowInf=as.numeric(learnedHowInf)) %>%
  mutate(timeLearnedCanSpread= # can spread ---
         case_when(
           doesKnow27When.r2=="before" ~ "0",
           doesKnow27When.r2=="june" ~ "0",
           doesKnow27When.r2=="july" ~ "1",
           doesKnow27When.r2=="august" ~ "2",
           doesKnow27When.r2=="september" ~ "3",
           doesKnow27When.r2=="october" ~ "4",
           doesKnow27When.r2=="november" ~ "5",
           doesKnow27When.r2=="december" ~ "6",
           doesKnow27When.r2=="january" ~ "7",

           doesKnow27When.r3=="before" ~ "8",
           doesKnow27When.r3=="february" ~ "8",
           doesKnow27When.r3=="march" ~ "9",
           doesKnow27When.r3=="april" ~ "10",
           doesKnow27When.r3=="may" ~ "11",
           doesKnow27When.r3=="june" ~ "12",
           doesKnow27When.r3=="july" ~ "13",

           TRUE ~ "13"
         ),
         timeLearnedCanSpread=as.numeric(timeLearnedCanSpread)) %>%
  mutate(learnedCanSpread=
           case_when(
             is.na(doesKnow27When.r2) & is.na(doesKnow27When.r3) ~ "0",
             TRUE ~ "1"
           ),
         learnedCanSpread=as.numeric(learnedCanSpread)) %>%
  mutate(Age=ifelse(c.age.r1>=12, "12+ years",
                    "<12 years")) %>%
  rowwise(.) %>%
  mutate(timeLearnedFull=max(timeLearnedHowInf, timeLearnedCanSpread)) %>%
  mutate(timeLearnedFull=ifelse(learnedFull==1, timeLearnedFull,
                                max(timeLearnedFull))) %>%
  select(Age, timeLearnedHIV, timeLearnedHowInf, timeLearnedCanSpread,
         learnedHIV, learnedHowInf, learnedCanSpread,
         learnedFull, timeLearnedFull)


# Figure B.4 - incidence plots by age =========================================
# HIV
hiv.fit.age <- survfit(Surv(timeLearnedHIV, learnedHIV)~Age,
                       data=datSurv)
summary(hiv.fit.age)

summary(survfit(Surv(datSurv$timeLearnedHIV, datSurv$learnedHIV)~1))

hiv.plot.age <-
  ggsurvplot(hiv.fit.age,
             conf.int=TRUE,
             pval=TRUE,
             fun=function(x) {1 - x},
             ylim=c(0,1),
             break.time.by=c(6),
             palette=c(gray.colors(7)[2], blue),
             xlab="Months Since Baseline",
             ylab="Cumulative disclosure",
             title="Knows HIV")

# how infected
howInf.fit.age <- survfit(Surv(timeLearnedHowInf, learnedHowInf)~Age,
                          data=datSurv)
summary(howInf.fit.age)

howInf.plot.age <-
  ggsurvplot(howInf.fit.age,
             conf.int=TRUE,
             pval=TRUE,
             fun=function(x) {1 - x},
             ylim=c(0,1),
             break.time.by=c(6),
             palette=c(gray.colors(7)[2], blue),
             xlab="Months Since Baseline",
             ylab="Cumulative disclosure",
             title="Knows HIV & How Infected")


# can spread
canSpread.fit.age <- survfit(Surv(timeLearnedCanSpread, learnedCanSpread)~Age,
                             data=datSurv)
summary(canSpread.fit.age)

canSpread.plot.age <-
  ggsurvplot(canSpread.fit.age,
             conf.int=TRUE,
             pval=TRUE,
             fun=function(x) {1 - x},
             ylim=c(0,1),
             break.time.by=c(6),
             palette=c(gray.colors(7)[2], blue),
             xlab="Months Since Baseline",
             ylab="Cumulative disclosure",
             title="Knows HIV & Can Spread")

# full disclosure
full.fit.age <- survfit(Surv(timeLearnedFull, as.numeric(learnedFull))~Age,
                        data=datSurv)
summary(full.fit.age)

full.plot.age <-
  ggsurvplot(full.fit.age,
             conf.int=TRUE,
             pval=TRUE,
             fun=function(x) {1 - x},
             ylim=c(0,1),
             break.time.by=c(6),
             palette=c(gray.colors(7)[2], blue),
             xlab="Months Since Baseline",
             ylab="Cumulative disclosure",
             title="Full Disclosure")

splots.age.list <- list()
splots.age.list[[1]] <- hiv.plot.age
splots.age.list[[2]] <- howInf.plot.age
splots.age.list[[3]] <- canSpread.plot.age
splots.age.list[[4]] <- full.plot.age

splots.age <-
  arrange_ggsurvplots(splots.age.list,
                      print=FALSE,
                      nrow=2,
                      ncol=2, surv.plot.height = 1)

ggsave("public/reports/plos2019/output/figures/incidencePlotsByAge.pdf", splots.age)

# Figure 3 - incidence plots ==================================================
# HIV
hiv.fit <- survfit(Surv(timeLearnedHIV, learnedHIV)~1,
                   data=datSurv)
summary(hiv.fit)

hiv.plot<-
  ggsurvplot(hiv.fit,
             conf.int=TRUE,
             #pval=TRUE,
             fun=function(x) {1 - x},
             ylim=c(0,1),
             break.time.by=c(2),
             palette="black",
             xlab="Months Since Baseline",
             ylab="Cumulative disclosure",
             title="Knows HIV")

# how infected
howInf.fit <- survfit(Surv(timeLearnedHowInf, learnedHowInf)~1,
                      data=datSurv)
summary(howInf.fit)

howInf.plot <-
  ggsurvplot(howInf.fit,
             conf.int=TRUE,
             #pval=TRUE,
             fun=function(x) {1 - x},
             ylim=c(0,1),
             break.time.by=c(2),
             palette="black",
             xlab="Months Since Baseline",
             ylab="Cumulative disclosure",
             title="Knows HIV & How Infected")


# can spread
canSpread.fit <- survfit(Surv(timeLearnedCanSpread, learnedCanSpread)~1,
                         data=datSurv)
summary(canSpread.fit)

canSpread.plot <-
  ggsurvplot(canSpread.fit,
             conf.int=TRUE,
             #pval=TRUE,
             fun=function(x) {1 - x},
             ylim=c(0,1),
             break.time.by=c(2),
             palette="black",
             xlab="Months Since Baseline",
             ylab="Cumulative disclosure",
             title="Knows HIV & Can Spread")

# full disclosure
full.fit <- survfit(Surv(timeLearnedFull, as.numeric(learnedFull))~1,
                    data=datSurv)
summary(full.fit)

full.plot <-
  ggsurvplot(full.fit,
             conf.int=TRUE,
             #pval=TRUE,
             fun=function(x) {1 - x},
             ylim=c(0,1),
             break.time.by=c(2),
             palette="black",
             xlab="Months Since Baseline",
             ylab="Cumulative disclosure",
             title="Full Disclosure")

splots <- list()
splots[[1]] <- hiv.plot
splots[[2]] <- howInf.plot
splots[[3]] <- canSpread.plot
splots[[4]] <- full.plot

splots.noAge <-
  arrange_ggsurvplots(splots,
                      print=FALSE,
                      nrow=2,
                      ncol=2,
                      surv.plot.height=1)

ggsave("public/reports/plos2019/output/figures/incidencePlots.pdf", splots.noAge)

ggsave("public/reports/plos2019/output/figures/incidencePlots.tiff",
       splots.noAge,
       width=10,
       height=10,
       units="in",
       dpi=300)


# Figure B.5 - disclosure by month ============================================

p.disc.HIV <-
  data.frame(time=hiv.fit$time,
             hiv.risk=hiv.fit$n.risk,
             hiv.event=hiv.fit$n.event,
             hiv.pdisc=hiv.fit$n.event/hiv.fit$n.risk)
p.disc.HowInf <-
  data.frame(time=howInf.fit$time,
             howInf.risk=howInf.fit$n.risk,
             howInf.event=howInf.fit$n.event,
             howInf.pdisc=howInf.fit$n.event/howInf.fit$n.risk)
p.disc.CanSpread <-
  data.frame(time=canSpread.fit$time,
             canSpread.risk=canSpread.fit$n.risk,
             canSpread.event=canSpread.fit$n.event,
             canSpread.pdisc=canSpread.fit$n.event/canSpread.fit$n.risk)
p.disc.full <-
  data.frame(time=full.fit$time,
             full.risk=full.fit$n.risk,
             full.event=full.fit$n.event,
             full.pdisc=full.fit$n.event/full.fit$n.risk)

p.disc <-
  full_join(p.disc.HIV,
            p.disc.HowInf,
            by="time")
p.disc <-
  full_join(p.disc,
            p.disc.CanSpread,
            by="time")
p.disc <-
  full_join(p.disc,
            p.disc.full,
            by="time")

discRisk <-
  p.disc %>%
  select(ends_with(".pdisc"), hiv.risk, time) %>%
  melt(., id=c("time", "hiv.risk")) %>%
  mutate(variable=ifelse(variable=="hiv.pdisc", "Knows HIV Status",
                  ifelse(variable=="howInf.pdisc", "Knows HIV & How Infected",
                  ifelse(variable=="canSpread.pdisc", "Knows HIV & Can Spread",
                  ifelse(variable=="full.pdisc", "Full Disclosure", "WTF"))))) %>%
  mutate(variable=factor(variable,
                         levels=c("Knows HIV Status",
                                  "Knows HIV & How Infected",
                                  "Knows HIV & Can Spread",
                                  "Full Disclosure"))) %>%
  filter(variable=="Knows HIV Status") %>%
  ggplot(., aes(x=time, y=value, label=hiv.risk)) +
  geom_line() +
  geom_point() +
  geom_text(size=3, vjust=-2) +
  theme_bw() +
  #facet_wrap(~variable) +
  labs(y="Proportion of children who learned their HIV status",
       x="Months Since Baseline") +
  ylim(0,1) +
  scale_x_continuous(breaks=c(0:13),
                     labels=c(0:13)) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  theme(axis.title.y=element_text(size=12))
cairo_pdf(paste("public/reports/plos2019/output/figures/discRisk", "pdf", sep="."),
          width=9, height=5)
print(discRisk)
dev.off()


