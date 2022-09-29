###------FIGURE Flow Diagram-----
## @knitr figureflowdiagram


# library(Gmisc, quietly = TRUE)
# library(glue)
# library(htmlTable)
# library(grid)
# library(magrittr)

## All records from WOS
recs_search <- vroom(list.files("../R/DATA-RAW/Clarvariate", full.names = T)) %>%
  mutate(YsincePub = 2023-PY,
         CitesPerYear = TC/YsincePub)

## At least 4 citations per year and contains a DOI
documents_4peryear <- recs_search %>%
  filter(CitesPerYear >=4)

## Documents review
documents_reviewed <- read.xlsx("./R/DATA-RAW/papers2.xlsx") %>%
  filter(Summary != "N/A")


## Abstract contains certain key words
documents_4pyearandAgingterms <- documents_4peryear %>%
  filter(grepl('\\baging\\b|\\baged\\b|elderly', AB))

fontsizes <- 8
articles_total <- boxGrob(glue("Articles identified through \nWeb of Science",
                               "n = {pop}",
                               pop = txtInt(nrow(recs_search)),
                               .sep = "\n"),
                          txt_gp = gpar(fontsize = fontsizes))
articles_4peryear <- boxGrob(glue("Articles with at least 4 cites/year",
                                  "n = {pop}",
                                  pop = txtInt(nrow(documents_4peryear)),
                                  .sep = "\n"),
                             txt_gp = gpar(fontsize = fontsizes))
articles_fulltext <- boxGrob(glue("Retrieved full text",
                                  "n = {incl}",
                                  incl = txtInt(nrow(documents_4pyearandAgingterms)),
                                  .sep = "\n"),
                             txt_gp = gpar(fontsize = fontsizes))
articles_included <- boxGrob(glue("Articles included in \nSystetmatic Review",
                                  "n = {recr}",
                                  recr = txtInt(nrow(documents_reviewed)),
                                  .sep = "\n"),
                             txt_gp = gpar(fontsize = fontsizes))



excluded_abstract <- boxGrob(glue("Does not contain aging-related \nterms in abstract",
                                  "n = {tot}",
                                  # " - not interested: {uninterested}",
                                  # " - contra-indicated: {contra}",
                                  tot = txtInt(nrow(documents_4peryear) - nrow(documents_4pyearandAgingterms)),
                                  # uninterested = 12,
                                  # contra = 30 - 12,
                                  .sep = "\n"),
                             txt_gp = gpar(fontsize = fontsizes))

excluded_review <- boxGrob(glue("Removed during document review \n(e.g. 'aging forests')",
                                "n = {tot}",
                                tot =txtInt(nrow(documents_4pyearandAgingterms) - nrow(documents_reviewed)),
                                
                                .sep = "\n"),
                           txt_gp = gpar(fontsize = fontsizes))

grid.newpage()
vert <- spreadVertical(articles_total,
                       articles_4peryear = articles_4peryear,
                       articles_fulltext = articles_fulltext,
                       articles_included = articles_included)


excluded <- moveBox(excluded_abstract,
                    x = .8,
                    y = coords(vert$articles_fulltext)$top + distance(vert$articles_4peryear, vert$articles_fulltext, half = TRUE, center = FALSE))

excluded2 <- moveBox(excluded_review,
                     x= 0.8,
                     y= coords(vert$articles_included)$top + distance(vert$articles_fulltext, vert$articles_included, half=TRUE, center=FALSE))

for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}




# Print boxes
vert
excluded
excluded2

connectGrob(vert$articles_4peryear, excluded, type = "L")
connectGrob(vert$articles_fulltext, excluded2, type = "L")