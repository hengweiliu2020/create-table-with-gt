
library(haven)
library(dplyr)
library(tidyr)
library(gt)

the_date <- as.character(Sys.Date())

adsl <- read_sas("C:\\ae\\adsl.sas7bdat")
adae <- read_sas("C:\\ae\\adae.sas7bdat")

# create adsl4 with treatment variable grp

adsl1 <- adsl[(adsl$trt01p=='Placebo' & adsl$SAFFL=='Y'), ]
adsl1$grp <- 'grp1'

adsl2 <- adsl[(adsl$trt01p=='Active' & adsl$SAFFL=='Y'), ]
adsl2$grp <- 'grp2'

adsl3 <- rbind(adsl1, adsl2)
adsl3$grp <- 'grp3'

adsl4 <- rbind(adsl1, adsl2, adsl3)

# create adae4 with treatment variable grp 

adae1 <- adae[(adae$trt01p=='Placebo' & adae$SAFFL=='Y' & adae$TRTEMFL=='Y'), ]
adae1$grp <- 'grp1'

adae2 <- adae[(adae$trt01p=='Active' & adae$SAFFL=='Y' & adae$TRTEMFL=='Y'), ]
adae2$grp <- 'grp2'

adae3 <- rbind(adae1, adae2)
adae3$grp <- 'grp3'

adae4 <- rbind(adae1, adae2, adae3)

# get the big N in column headers from adsl4
bign <- table(group=adsl4$grp)

# get the number of subjects with at least one TEAE

group_by_grp <- 
  adae4 %>%                   
  group_by(grp) %>%
  summarise(unique_subj = n_distinct(USUBJID))


# get the count by System Organ class

group_by_grp1 <- 
  adae4 %>%                   
  group_by(grp, AEBODSYS) %>%
  summarise(unique_subj = n_distinct(USUBJID))


# get the count by preferred term 

group_by_grp2 <- 
  adae4 %>%                   
  group_by(grp, AEBODSYS, AEDECOD) %>%
  summarise(unique_subj = n_distinct(USUBJID))


# do the transpose 

a1 <- spread(group_by_grp, grp, unique_subj)
a2 <- spread(group_by_grp1, grp, unique_subj)
a3 <- spread(group_by_grp2, grp, unique_subj)
 
a1$term <- "Subjects with at least one TEAE"
a1$AEBODSYS <- " "
a2$term <- a2$AEBODSYS
a3$term <- a3$AEDECOD

a1 <- a1[c("AEBODSYS", "term", "grp1", "grp2", "grp3")]
a2 <- a2[c("AEBODSYS", "term", "grp1", "grp2", "grp3")]
a3 <- a3[c("AEBODSYS", "term", "grp1", "grp2", "grp3")]

final <- rbind(a1, a2, a3)

#sort the ae data by AEBODSYS and descending order in the total column
df <-final[order(final$AEBODSYS, -final$grp3),]

#create the final data df for reporting

df$perc1 <- 100*df$grp1/bign[1]
df$perc1 <- format(round(df$perc1, 1), nsmall = 1)
df$grp1_c <- paste(df$grp1, "(", df$perc1, ")")
df$grp1_c <- ifelse(is.na(df$grp1),"0", df$grp1_c)

df$perc2 <- 100*df$grp2/bign[2]
df$perc2 <- format(round(df$perc2, 1), nsmall = 1)
df$grp2_c <- paste(df$grp2, "(", df$perc2, ")")
df$grp2_c <- ifelse(is.na(df$grp2),"0", df$grp2_c)

df$perc3 <- 100*df$grp3/bign[3] 
df$perc3 <- format(round(df$perc3, 1), nsmall = 1)
df$grp3_c <- paste(df$grp3, "(", df$perc3, ")")
df$grp3_c <- ifelse(is.na(df$grp3),"0", df$grp3_c)

df <- df[c("AEBODSYS", "term", "grp1_c","grp2_c","grp3_c")]

# use gt to do the reporting 
tab_html <- df %>% 
  gt() %>%
  
  tab_header(
    title = "Table 14.3.1 Treatment Emergent Adverse Events by System Organ Class and Preferred Term",
    subtitle = "Safety Population"
  ) %>%
  tab_source_note(
    source_note = "Note: TEAE is defined to be the AEs with start date >= first dose date and <= last dose date + 30."
  ) %>%
  tab_source_note(
    source_note = "System organ class is in light gray. The table is sorted by system organ class and descending order in the total column."
  ) %>%
  
  tab_source_note(
    source_note = paste('Program Source: ae.R            Executed: (Draft)',  the_date)
  ) %>%
  
  cols_label(
    term = html("System Organ Class <br> Preferred Term"),
    grp1_c = html(paste("Placebo <br> (N=", bign[1], ")")),
    grp2_c = html(paste("Active <br> (N=", bign[2], ")")),
    grp3_c = html(paste("Total <br> (N=", bign[3], ")"))
  ) %>%
  
  tab_options(
    table.border.top.color = "white",
    heading.border.bottom.color = "black",
    table.border.bottom.color = "white",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "white", 
    row_group.border.bottom.color = "white", 
    row_group.border.top.color = "white", 
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
  ) %>%
  
  tab_style(
  style = list(
    cell_fill(color = "#D3D3D3")
  ),
  locations = cells_body(
    
    rows = AEBODSYS==term
  )
) %>%
  cols_hide(
    columns = c(AEBODSYS)
  ) 

# output the HTML table

tab_html %>%
  gtsave("teae_soc_pt.html", path = "C:\\ae" )
















