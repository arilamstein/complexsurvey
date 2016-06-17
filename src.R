# thomasyokota[at]gmail.com

# DEPENDENCIES -----------------------------------------------------------------
install.packages('pacman')
pacman::p_load(RCurl, foreign, downloader, survey, srvyr, ggplot2, dplyr)

# DATA -------------------------------------------------------------------------
source_url("https://raw.githubusercontent.com/ajdamico/asdfree/master/Download%20Cache/download%20cache.R", prompt=F, echo=F)
# download ez-pz brought to you by anthony joseph damico [ajdamico@gmail.com]

tf <- tempfile(); td <- tempdir()
xpt <- "http://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.ZIP"
download_cached(xpt, tf, mode='wb')
local.fn <- unzip(tf, exdir=td)

brfss14 <- read.xport(local.fn)
save(brfss14, file="brfss14.rda")
# load("brfss14.rda")

state.id <- read.csv("https://raw.githubusercontent.com/tyokota/complexsurvey/master/brfss_state.csv", stringsAsFactors=F)
ageg65yr.id <- read.csv("https://raw.githubusercontent.com/tyokota/complexsurvey/master/ageg65yr.csv", stringsAsFactors=F)


# ANALYSIS----------------------------------------------------------------------
brfss14 <- brfss14 %>%
  mutate(X_BMI5CAT2 = car::recode(X_BMI5CAT, "c(3,4)=1; NA=NA;  else=0"))

brfss.design14 <- brfss14 %>%
  as_survey_design(ids=X_PSU, weight=X_LLCPWT, nest=TRUE, strata=X_STSTR, variables= c(X_BMI5CAT2, X_MRACE1, X_STATE))

# options(survey.lonely.psu = "certainty")
options(survey.lonely.psu = "adjust")

brfss.design14 <- brfss.design14 %>%
  mutate(X_BMI5CAT2=as.factor(X_BMI5CAT2))

BMI5CAT2.1 <- brfss.design14 %>%
  group_by(X_STATE, X_BMI5CAT2) %>%
  summarize(prevalence = survey_mean(na.rm=T),
            N = survey_total(na.rm=T))

BMI5CAT2.1 <- BMI5CAT2.1 %>%
  mutate(X_STATE = as.character(X_STATE)) %>%
  left_join(state.id, by=c("X_STATE"="VALUE"))

BMI5CAT2.1 %>% filter(X_STATE==15)
BMI5CAT2.1 %>% filter(X_STATE==6)

png("state_overweightobese.png", height=500, width=770)
BMI5CAT2.1 %>%
  filter(X_BMI5CAT2==1) %>%
  ggplot(aes(x=reorder(STATE, prevalence), y=prevalence)) +
  scale_y_continuous(labels=scales::percent) +
  geom_bar(stat="identity") +
  ylab("YES (%)") +
  xlab("STATE") +
  ggtitle("Computed body mass index categories (overweight or obese) by state") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = 'white' )) +
  guides(fill=FALSE)
dev.off()

BMI5CAT2.2 <- BMI5CAT2.1 %>%
  filter(X_BMI5CAT2==1) %>%
  select(region=STATE, value=prevalence) %>%
  mutate(region = tolower(region))

png("chorplethr.png", height=500, width=770)
choroplethr::state_choropleth(BMI5CAT2.2, title="Computed body mass index categories (overweight or obese) choropleth map",
                              num_colors=9)
dev.off()

 # note: hawaii-only
brfss14a <- brfss14 %>%
  filter(X_STATE==15)

brfss.design14a <- brfss14a %>%
  as_survey_design(ids=X_PSU, weight=X_LLCPWT, nest=TRUE, strata=X_STSTR, variables= c(X_BMI5CAT2, X_MRACE1, X_STATE, X_AGEG5YR))

brfss.design14a <- brfss.design14a %>%
  mutate(X_BMI5CAT2a=car::recode(X_BMI5CAT2, "1='Obese/overweight'; else='Not Obese/overweight'"),
         X_BMI5CAT2a=factor(X_BMI5CAT2a, levels=c('Obese/overweight', 'Not Obese/overweight'), ordered=TRUE),
         X_MRACE1a=car::recode(X_MRACE1, "1='White'; 2='Black'; 3='AIAN'; 4='Asian'; 5='NHOPI'; c(6,7)='other/multiracial'; else=NA"),
         X_AGEG5YR=as.factor(X_AGEG5YR))

BMI5CAT2.3 <- brfss.design14a %>%
  group_by(X_STATE, X_MRACE1a, X_BMI5CAT2a) %>%
  summarize(prevalence = survey_mean(na.rm=T),
            N = survey_total(na.rm=T),
            prevalence_ci = survey_mean(na.rm=T, vartype = c("ci"))) %>%
  mutate(X_STATE = as.character(X_STATE)) %>%
  left_join(state.id, by=c("X_STATE"="VALUE"))

png("hawaiirace.png", height=500, width=770)
BMI5CAT2.3 %>%
  ggplot(aes(x = X_MRACE1a, y = prevalence, group = X_BMI5CAT2a)) +
    geom_bar(stat = "identity", aes(fill = X_BMI5CAT2a), alpha = 0.3) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Prevalence (%)") +
  xlab("Race") +
  ggtitle("Computed body mass index categories (overweight or obese) in Hawaii by race") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = 'white')) +
  guides(fill = guide_legend(title = "Categories"))
dev.off()

BMI5CAT2.4 <- brfss.design14a %>%
  group_by(X_STATE, X_MRACE1a, X_AGEG5YR, X_BMI5CAT2a) %>%
  summarize(prevalence = survey_mean(na.rm=T),
            N = survey_total(na.rm=T),
            prevalence_ci = survey_mean(na.rm=T, vartype = c("ci"))) %>%
  mutate(X_STATE = as.character(X_STATE)) %>%
  left_join(state.id, by=c("X_STATE"="VALUE")) %>%
  mutate(X_STATE = as.character(X_STATE)) %>%
  left_join(ageg65yr.id %>% rename(X_AGEG5YR2=X_AGEG5YR), by=c("X_AGEG5YR"="VALUE"))

png("hawaiiraceage.png", height=500, width=770)
BMI5CAT2.4 %>%
  filter(X_MRACE1a=="NHOPI" | X_MRACE1a=="White" | X_MRACE1a=="Asian", X_AGEG5YR2!="Dont know/Refused/Missing") %>%
  ggplot(aes(X_AGEG5YR2, prevalence)) +
    geom_bar(stat = "identity", aes(fill = X_BMI5CAT2a), col = "gray", alpha = 0.7) +
    facet_grid(~X_MRACE1a) +
    scale_y_continuous(labels=scales::percent) +
    ylab("Prevalence (%)") +
    xlab("Age Group") +
    ggtitle("Computed body mass index categories (overweight or obese) in Hawaii by race and age") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_rect(fill = 'white' )) +
    guides(fill = guide_legend(title = "Categories"))
dev.off()
