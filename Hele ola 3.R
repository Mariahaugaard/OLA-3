#Pakker i til besvarelse af opgave 1
library(dkstat)
library(tidyr)
library(dplyr)
library(ggplot2)

# --------------------------------------------------
# Opgave 1,1 & 1,2 & 1,3: Den bedste mikroøkonomiske indikator
# --------------------------------------------------

forv1_meta <- dst_meta(table = "FORV1", lang = "da")

#Hent navnene på alle indikatorerne i FORV1
spoergsmaal <- forv1_meta$values$INDIKATOR$text

#Fjern F1 & F11
spoergsmaal <- spoergsmaal[
  !grepl("Forbrugertillidsindikatoren|Anser det som fornuftigt", spoergsmaal)
]

forv1 <- dst_get_data(
  table = "FORV1",
  query = list(
    INDIKATOR = spoergsmaal,
    Tid = "*"
  ),
  meta_data = forv1_meta,
  lang = "da",
  parse_dst_tid = FALSE
)

# Behold januar 2000 og frem
forv1 <- forv1[forv1$TID >= "2000M01", ]

# Lav spørgsmålene til kolonner
forv1 <- pivot_wider(
  forv1,
  id_cols = TID,
  names_from = INDIKATOR,
  values_from = value
)

forv1 <- as.data.frame(forv1)

# Behold kun F-koden i spørgsmålenes kolonnenavne
names(forv1)[-1] <- sub(" .*", "", names(forv1)[-1])

rownames(forv1) <- NULL

# Hent spørgsmålsnumrene fra kolonnenavnene
numre <- as.numeric(sub("^F", "", names(forv1)[-1]))

# Behold TID først og sortér spørgsmålene efter nummer
forv1 <- forv1[, c(1, order(numre) + 1)]

View(forv1)

# Lav måneder til kvartaler
maaned <- as.numeric(substr(forv1$TID, 6, 7))

kvartal <- paste0(
  substr(forv1$TID, 1, 4),
  "K",
  ceiling(maaned / 3)
)

# Beregn gennemsnit pr. kvartal
forv1_kvt <- aggregate(
  forv1[, -1],
  by = list(TID = kvartal),
  FUN = mean
)

View(forv1_kvt)

# Vend fortegnende på F6 og F8
kolonner <- names(forv1_kvt) %in% c("F6", "F8")
forv1_kvt[, kolonner] <- -forv1_kvt[, kolonner]

# --------------------------------------------------
# For 1 spørgsmål - 11 kombinationer
# --------------------------------------------------

comb1 <- matrix(0, ncol=11, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb1[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],1)),nrow=1)),nrow=1)
}

dim(comb1)

# --------------------------------------------------
# For 2 spørgsmål - 55 kombinationer
# --------------------------------------------------

comb2 <- matrix(0, ncol=55, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb2[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],2)),nrow=2)),nrow=1)
}

dim(comb2)

# --------------------------------------------------
# For 3 spørgsmål - 165 kombinationer
# --------------------------------------------------

comb3 <- matrix(0, ncol=165, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb3[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],3)),nrow=3)),nrow=1)
}

dim(comb3)

# --------------------------------------------------
# For 4 spørgsmål - 330 kombinationer
# --------------------------------------------------

comb4 <- matrix(0, ncol=330, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb4[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],4)),nrow=4)),nrow=1)
}

dim(comb4)

# --------------------------------------------------
# For 5 spørgsmål - 462 kombinationer
# --------------------------------------------------

comb5 <- matrix(0, ncol=462, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb5[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],5)),nrow=5)),nrow=1)
}

dim(comb5)

# --------------------------------------------------
# For 6 spørgsmål - 462 kombinationer
# --------------------------------------------------

comb6 <- matrix(0, ncol=462, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb6[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],6)),nrow=6)),nrow=1)
}

dim(comb6)

# --------------------------------------------------
# For 7 spørgsmål - 330 kombinationer
# --------------------------------------------------

comb7 <- matrix(0, ncol=330, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb7[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],7)),nrow=7)),nrow=1)
}

dim(comb7)

# --------------------------------------------------
# For 8 spørgsmål - 165 kombinationer
# --------------------------------------------------

comb8 <- matrix(0, ncol=165, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb8[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],8)),nrow=8)),nrow=1)
}

dim(comb8)

# --------------------------------------------------
# For 9 spørgsmål - 55 kombinationer
# --------------------------------------------------

comb9 <- matrix(0, ncol=55, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb9[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],9)),nrow=9)),nrow=1)
}

dim(comb9)

# --------------------------------------------------
# For 10 spørgsmål - 11 kombinationer
# --------------------------------------------------

comb10 <- matrix(0, ncol=11, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb10[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],10)),nrow=10)),nrow=1)
}

dim(comb10)

# --------------------------------------------------
# For 11 spørgsmål - 1 kombination
# --------------------------------------------------

comb11 <- matrix(0, ncol=1, nrow=nrow(forv1_kvt))

for(i in 1:nrow(forv1_kvt)) {
  comb11[i,] <- matrix(colMeans(matrix(as.numeric(combn(forv1_kvt[i,2:ncol(forv1_kvt)],11)),nrow=11)),nrow=1)
}

dim(comb11)

# --------------------------------------------------
# Saml alle 2047 kombinationer
# --------------------------------------------------

comb_alle <- cbind(
  comb1, comb2, comb3, comb4, comb5, comb6,
  comb7, comb8, comb9, comb10, comb11
)

dim(comb_alle)
View(comb_alle)

#Kontroler at comb1-11 er sat korrekt sammen
identical(comb_alle[, 1:11], comb1)
identical(comb_alle[, 12:66], comb2)
identical(comb_alle[, 67:231], comb3)
identical(comb_alle[, 232:561], comb4)
identical(comb_alle[, 562:1023], comb5)
identical(comb_alle[, 1024:1485], comb6)
identical(comb_alle[, 1486:1815], comb7)
identical(comb_alle[, 1816:1980], comb8)
identical(comb_alle[, 1981:2035], comb9)
identical(comb_alle[, 2036:2046], comb10)
identical(comb_alle[, 2047, drop=FALSE], comb11)

#----------------------------------------------------
#Hent privatforbruget
#----------------------------

#Privatforbrug fra NKH1
forbrugMeta <- dst_meta("NKH1")

forbrugMeta$variables
str(forbrugMeta$values)

queryForbrug <- list(
  "P.31 Husholdningernes forbrugsudgifter",
  "2020-priser, kædede værdier",
  "Sæsonkorrigeret",
  "*"
)

names(queryForbrug) <- forbrugMeta$variables$id

forbrugRaw <- dst_get_data(
  "NKH1",
  query = queryForbrug,
  parse_dst_tid = FALSE
)

#Behold perioden 1999K1-2026K2
forbrugRaw <- forbrugRaw[
  forbrugRaw$TID >= "1999K1" &
    forbrugRaw$TID <= "2026K2",
]

#Sorter kvartalerne
forbrugRaw <- forbrugRaw[order(forbrugRaw$TID), ]

#Lav det samme format som Excel-filen havde
forbrug <- data.frame(
  t(forbrugRaw$value),
  check.names = FALSE
)

colnames(forbrug) <- forbrugRaw$TID

# Beregn årlig realvækst
realvaekst <- (forbrugRaw$value / 
                 dplyr::lag(forbrugRaw$value, 4) - 1) * 100

# Opret tabel med kvartal og realvækst
forbrug_vaekst <- data.frame(
  TID = forbrugRaw$TID,
  Realvaekst = realvaekst
)

View(forbrug_vaekst)

# Match kvartalerne fra comb_alle med privatforbruget
y <- forbrug_vaekst$Realvaekst[
  match(forv1_kvt$TID, forbrug_vaekst$TID)
]

# Kontrollér antal observationer
length(y)
nrow(comb_alle)

# Opret tom matrix til R²
R2 <- matrix(NA_real_, nrow=1, ncol=2047)

# Beregn R² for alle kombinationer
for(i in 1:ncol(comb_alle)) {
  model <- lm(y ~ comb_alle[,i])
  R2[1,i] <- summary(model)$r.squared
}

# Kontrollér dimensionerne
dim(R2)

# Vis alle R²-værdier
View(R2)

# Højeste R²
max(R2, na.rm=TRUE)

# Hvilken kombination har den højeste R²?
which.max(R2)

#Find spørgsmålene bag hver indikator
sp_navne <- names(forv1_kvt)[-1]

kombinationsnavne <- character(0)

for (k in 1:length(sp_navne)) {
  navne <- combn(
    sp_navne,
    k,
    FUN = function(x) paste(x, collapse = " + ")
  )
  
  kombinationsnavne <- c(kombinationsnavne, navne)
}

kombinationsnavne[which.max(R2)]

#----------------------------
#LOOPS
#----------------------------

# Fælles kvartaler uden manglende værdier
brug <- complete.cases(y, comb_alle)

y_stabil <- y[brug]
x_stabil <- comb_alle[brug, , drop = FALSE]
tid_stabil <- forv1_kvt$TID[brug]

# Sortér efter tid
raekkefoelge <- order(tid_stabil)

y_stabil <- y_stabil[raekkefoelge]
x_stabil <- x_stabil[raekkefoelge, , drop = FALSE]
tid_stabil <- tid_stabil[raekkefoelge]

# Antal kvartaler og indikatorer
n <- length(y_stabil)
antal <- ncol(x_stabil)

# Stop, når der er 20 kvartaler tilbage
min_kvartaler <- 20
stopifnot(n >= min_kvartaler)

omgange <- n - min_kvartaler + 1

# Gem R²: én række pr. omgang, én kolonne pr. indikator
R2_slut <- matrix(NA_real_, nrow = omgange, ncol = antal)
R2_start <- matrix(NA_real_, nrow = omgange, ncol = antal)

# Loop 1: Fjern fra slutningen
for (j in 1:omgange) {
  
  rækker <- 1:(n - j + 1)
  
  R2_slut[j, ] <- as.numeric(
    cor(y_stabil[rækker],
        x_stabil[rækker, , drop = FALSE])
  )^2
}


# Loop 2: Fjern fra begyndelsen
for (j in 1:omgange) {
  
  rækker <- j:n
  
  R2_start[j, ] <- as.numeric(
    cor(y_stabil[rækker],
        x_stabil[rækker, , drop = FALSE])
  )^2
}

# Loop 3: Fjern fra begge ender
omgange_begge <- floor((n - min_kvartaler) / 2) + 1

R2_begge <- matrix(
  NA_real_,
  nrow = omgange_begge,
  ncol = antal
)

for (j in 1:omgange_begge) {
  
  rækker <- j:(n - j + 1)
  
  R2_begge[j, ] <- as.numeric(
    cor(y_stabil[rækker],
        x_stabil[rækker, , drop = FALSE])
  )^2
}

# Rangér resultaterne fra loop 3
rang_begge <- matrix(
  NA_real_,
  nrow = omgange_begge,
  ncol = antal
)

for (j in 1:omgange_begge) {
  rang_begge[j, ] <- rank(-R2_begge[j, ], na.last = "keep")
}

stopifnot(!anyNA(rang_begge))

# Rangér resultaterne fra loop 1 og 2
rang_slut <- matrix(NA_real_, nrow = omgange, ncol = antal)
rang_start <- matrix(NA_real_, nrow = omgange, ncol = antal)

for (j in 1:omgange) {
  rang_slut[j, ] <- rank(-R2_slut[j, ], na.last = "keep")
  rang_start[j, ] <- rank(-R2_start[j, ], na.last = "keep")
}

stopifnot(!anyNA(rang_slut), !anyNA(rang_start))

# Saml rangeringerne fra alle tre loops
resultat <- data.frame(
  Indikator = kombinationsnavne,
  Rang_fjern_slut = colMeans(rang_slut),
  Rang_fjern_start = colMeans(rang_start),
  Rang_fjern_begge = colMeans(rang_begge)
)

# Hele perioden tælles kun én gang
resultat$Samlet_rang <- (
  colSums(rang_slut) +
    colSums(rang_start) +
    colSums(rang_begge) -
    2 * rang_slut[1, ]
) / (2 * omgange + omgange_begge - 2)

# Sortér efter bedste samlede rang
resultat <- resultat[order(resultat$Samlet_rang), ]

# Opdatér top 10 og tilføj R² for hele perioden
top10 <- head(resultat, 10)

numre_top10 <- match(top10$Indikator, kombinationsnavne)
top10$R2 <- as.numeric(R2)[numre_top10]

top10$Korrelation <- as.numeric(
  cor(y_stabil, x_stabil[, numre_top10, drop = FALSE])
)

View(top10)

# Top 100: kun spørgsmålene
top100 <- resultat[1:100, "Indikator", drop = FALSE]

# Tæl spørgsmålene i hver indikator
top100$Antal_spoergsmaal <- lengths(
  strsplit(top100$Indikator, " + ", fixed = TRUE)
)

View(top100)

# Gennemsnitligt antal spørgsmål
mean(top100$Antal_spoergsmaal)

# Hvor mange indikatorer har fx 3, 4 eller 5 spørgsmål?
table(top100$Antal_spoergsmaal)

#----------------------------
#GRAFER
#----------------------------

#---------------------------------------------
# Graf 1: Antal spørgsmål i indikatorerne i top 100
#---------------------------------------------

fordeling <- table(
  factor(top100$Antal_spoergsmaal, levels = 1:11)
)

# Fremhæv indikatorer med 5–7 spørgsmål
farver <- rep("grey80", 11)
farver[5:7] <- "steelblue"

andel <- sum(
  top100$Antal_spoergsmaal %in% 5:7
) / nrow(top100) * 100

gennemsnit <- format(
  round(mean(top100$Antal_spoergsmaal), 1),
  decimal.mark = ",",
  nsmall = 1
)

par(mar = c(7, 4, 5, 2))

barplot(
  fordeling,
  col = farver,
  border = NA,
  main = paste0(
    round(andel),
    "% af de 100 bedst rangerede indikatorer indeholder 5–7 spørgsmål"
  ),
  xlab = "Antal spørgsmål pr. indikator",
  ylab = "Antal indikatorer",
  ylim = c(0, max(fordeling) + 5),
  las = 1
)

mtext(
  paste0(
    "Udvalgt efter gennemsnitlig rangering | Gennemsnit: ",
    gennemsnit,
    " spørgsmål"
  ),
  side = 3,
  line = 0.3,
  cex = 0.8
)

mtext(
  "Kilde: Danmarks Statistik, FORV1 og NKH1, samt egne beregninger.",
  side = 1,
  line = 5,
  adj = 0,
  cex = 0.75
)


# Find analyseperioden i jeres data
periode <- paste(min(tid_stabil), max(tid_stabil), sep = "–")

# Tilføj note under grafen
mtext(
  paste0(
    "Analyseperiode: ", periode,
    ". Delperioder med mindst ", min_kvartaler, " kvartaler."
  ),
  side = 1,
  line = 4,
  adj = 0,
  cex = 0.75
)

# --------------------------------------------------
# Graf 2: Spørgsmålenes forekomst i top 100
# --------------------------------------------------

# Del indikatorerne op i enkelte spørgsmål
sp_top100 <- unlist(
  strsplit(top100$Indikator, " + ", fixed = TRUE)
)

# Tæl alle spørgsmål, også dem med 0 forekomster
hyppighed <- table(
  factor(sp_top100, levels = sp_navne)
)

# Sortér fra hyppigst til mindst hyppigt
hyppighed <- sort(hyppighed, decreasing = TRUE)

# Fremhæv F4 og F7
farver <- ifelse(
  names(hyppighed) %in% c("F4", "F7"),
  "steelblue",
  "grey80"
)

# Find analyseperioden
periode <- paste(
  min(tid_stabil),
  max(tid_stabil),
  sep = "–"
)

# Giv plads til titel, noter og kilde
gamle_indstillinger <- par(no.readonly = TRUE)
par(mar = c(9, 5, 5, 2))

# Tegn grafen
placeringer <- barplot(
  hyppighed,
  col = farver,
  border = NA,
  main = "F4 og F7 indgår i næsten alle indikatorer i top 100",
  cex.main = 0.95,
  xlab = "Spørgsmål",
  ylab = "Andel af top 100 (%)",
  ylim = c(0, 105),
  las = 1
)

# Vis antal over søjlerne, også nulværdier
# Vis procent over hver søjle
text(
  x = placeringer,
  y = as.numeric(hyppighed),
  labels = paste0(as.numeric(hyppighed), " %"),
  pos = 3,
  cex = 0.8
)

# Undertitel
mtext(
  "Forekomst blandt de 100 bedst rangerede indikatorer",
  side = 3,
  line = 0.3,
  cex = 0.8
)

# Forklar de fremhævede spørgsmål
mtext(
  "F4: Danmarks økonomi i dag sammenlignet med for et år siden.",
  side = 1,
  line = 4,
  adj = 0,
  cex = 0.7
)

mtext(
  "F7: Forventet prisudvikling om et år sammenlignet med i dag.",
  side = 1,
  line = 5,
  adj = 0,
  cex = 0.7
)

# Analyseperiode
mtext(
  paste0(
    "Analyseperiode: ", periode,
    ". Delperioder med mindst ", min_kvartaler, " kvartaler."
  ),
  side = 1,
  line = 6,
  adj = 0,
  cex = 0.7
)

# Kilde
mtext(
  "Kilde: Danmarks Statistik, FORV1 og NKH1, samt egne beregninger.",
  side = 1,
  line = 7,
  adj = 0,
  cex = 0.7
)

# Gendan grafikindstillinger
par(gamle_indstillinger)

# --------------------------------------------------
# Graf 3: R² og korrelation for top 10
# --------------------------------------------------

# Find indikatorerne i datasættet
numre_top10 <- match(top10$Indikator, kombinationsnavne)

# Beregn på samme fælles analyseperiode
top10$Korrelation <- as.numeric(
  cor(y_stabil, x_stabil[, numre_top10, drop = FALSE])
)

top10$R2 <- top10$Korrelation^2

# Nummer 1 skal stå øverst
vis <- nrow(top10):1

vaerdier <- rbind(
  top10$R2[vis],
  top10$Korrelation[vis]
)

etiketter <- paste0(
  vis, ". ",
  top10$Indikator[vis]
)

# Dansk decimalkomma
tal <- matrix(
  formatC(
    as.vector(vaerdier),
    format = "f",
    digits = 3,
    decimal.mark = ","
  ),
  nrow = 2
)

periode <- paste(min(tid_stabil), max(tid_stabil), sep = "–")

gamle_indstillinger <- par(no.readonly = TRUE)
par(mar = c(8, 19, 6, 2))

placeringer <- barplot(
  vaerdier,
  beside = TRUE,
  horiz = TRUE,
  names.arg = etiketter,
  col = c("steelblue", "darkorange"),
  border = NA,
  main = "Top 10: Forklaringsgrad og korrelation",
  cex.main = 0.95,
  xlab = "R² og korrelation",
  xlim = c(if (any(vaerdier < 0)) -1.15 else 0, 1.15),
  las = 1,
  cex.names = 0.65
)

abline(v = 0, col = "grey60")

# Vis værdier ved søjlernes ender
text(
  x = as.vector(vaerdier),
  y = as.vector(placeringer),
  labels = as.vector(tal),
  pos = ifelse(as.vector(vaerdier) < 0, 2, 4),
  cex = 0.7
)

mtext(
  "Udvalgt efter gennemsnitlig rangering på tværs af delperioder",
  side = 3,
  line = 0.3,
  cex = 0.75
)

legend(
  "top",
  inset = c(0, -0.14),
  legend = c("R²", "Korrelation"),
  fill = c("steelblue", "darkorange"),
  horiz = TRUE,
  bty = "n",
  xpd = NA,
  cex = 0.8
)

mtext(
  paste0("R² og korrelation beregnet for hele perioden: ", periode, "."),
  side = 1, line = 4, adj = 0, cex = 0.7
)

mtext(
  "R² = forklaret variation. Korrelation = sammenhængens retning og styrke.",
  side = 1, line = 5, adj = 0, cex = 0.7
)

mtext(
  "Kilde: Danmarks Statistik, FORV1 og NKH1, samt egne beregninger.",
  side = 1, line = 6, adj = 0, cex = 0.7
)

par(gamle_indstillinger)

# --------------------------------------------------
# Graf 4 med korrelation: Top 3 med R² og korrelation
# --------------------------------------------------

top3 <- head(top10, 3)

# Beregn på den fælles analyseperiode
numre_top3 <- match(top3$Indikator, kombinationsnavne)

top3$Korrelation <- as.numeric(
  cor(y_stabil, x_stabil[, numre_top3, drop = FALSE])
)

top3$R2 <- top3$Korrelation^2

# Antal spørgsmål
antal_sp_top3 <- lengths(
  strsplit(top3$Indikator, " + ", fixed = TRUE)
)

# Nummer 1 øverst
vis_top3 <- nrow(top3):1

# To søjler pr. indikator
vaerdier_top3 <- rbind(
  top3$R2[vis_top3],
  top3$Korrelation[vis_top3]
)

# R² med procenttegn og korrelation med decimalkomma
tal_top3 <- rbind(
  paste0(
    formatC(
      top3$R2[vis_top3] * 100,
      format = "f",
      digits = 1,
      decimal.mark = ","
    ),
    " %"
  ),
  formatC(
    top3$Korrelation[vis_top3],
    format = "f",
    digits = 3,
    decimal.mark = ","
  )
)

periode_top3 <- paste(
  min(tid_stabil),
  max(tid_stabil),
  sep = "–"
)

# Plads til titel, signaturforklaring og noter
gamle_indstillinger_top3 <- par(no.readonly = TRUE)
par(mar = c(7, 19, 8, 2))

placeringer_top3 <- barplot(
  vaerdier_top3,
  beside = TRUE,
  horiz = TRUE,
  space = c(0.15, 1.2),
  col = c("steelblue", "darkorange"),
  border = NA,
  xlim = c(if (any(vaerdier_top3 < 0)) -1 else 0, 1),
  axes = FALSE,
  axisnames = FALSE,
  xlab = "R² og korrelation",
  cex.lab = 0.9
)

# Akse uden procenttegn
akse_top3 <- if (any(vaerdier_top3 < 0)) {
  seq(-1, 1, 0.2)
} else {
  seq(0, 1, 0.2)
}

axis(
  side = 1,
  at = akse_top3,
  labels = formatC(
    akse_top3,
    format = "f",
    digits = 1,
    decimal.mark = ","
  ),
  cex.axis = 0.85
)

# Midten af hvert søjlepar
midter_top3 <- colMeans(placeringer_top3)

# Placering i fed skrift
venstre_top3 <- par("usr")[1] - 0.04

text(
  x = venstre_top3,
  y = midter_top3 + 0.55,
  labels = paste0(vis_top3, ". plads"),
  adj = 1,
  font = 2,
  cex = 0.95,
  xpd = NA
)

# Kombinationen
text(
  x = venstre_top3,
  y = midter_top3,
  labels = top3$Indikator[vis_top3],
  adj = 1,
  cex = 0.75,
  xpd = NA
)

# Antal spørgsmål
text(
  x = venstre_top3,
  y = midter_top3 - 0.5,
  labels = paste0(
    "(", antal_sp_top3[vis_top3], " spørgsmål)"
  ),
  adj = 1,
  cex = 0.7,
  col = "grey40",
  xpd = NA
)

# Tal ved søjlernes ender
text(
  x = as.vector(vaerdier_top3),
  y = as.vector(placeringer_top3),
  labels = as.vector(tal_top3),
  pos = ifelse(as.vector(vaerdier_top3) < 0, 2, 4),
  cex = 0.85
)

# Titel
mtext(
  "Top 3 forklarer omkring halvdelen af\nvariationen i forbrugsvæksten",
  side = 3,
  line = 4.5,
  font = 2,
  cex = 1
)

# Undertitel
mtext(
  "Placering efter gennemsnitlig rangering på tværs af delperioder",
  side = 3,
  line = 2.3,
  cex = 0.75
)

# Signaturforklaring over grafen
legend(
  x = mean(par("usr")[1:2]),
  y = par("usr")[4],
  legend = c("R²", "Korrelation (r)"),
  fill = c("steelblue", "darkorange"),
  horiz = TRUE,
  bty = "n",
  xjust = 0.5,
  yjust = 0,
  xpd = NA,
  cex = 0.8
)

# Kort note
mtext(
  paste0(
    "R² og korrelation: ", periode_top3,
    ". Placeringerne følger rangeringen på tværs af delperioder."
  ),
  side = 1,
  line = 4,
  adj = 0,
  cex = 0.65
)

# Kilde
mtext(
  "Kilde: Danmarks Statistik, FORV1 og NKH1; egne beregninger.",
  side = 1,
  line = 5,
  adj = 0,
  cex = 0.7
)

par(gamle_indstillinger_top3)


# --------------------------------------------------
# Graf 5: R² for de fire indikatorer
# DI, CCI, Danmarks Statistik og egen indikator
# --------------------------------------------------

# DI og CCI består hver af fire spørgsmål
DI_sp <- c("F2", "F4", "F9", "F10")
CCI_sp <- c("F2", "F3", "F9", "F10")

# Beregn indikatorerne som simple gennemsnit
DI <- rowMeans(forv1_kvt[, DI_sp])
CCI <- rowMeans(forv1_kvt[, CCI_sp])


# --------------------------------------------------
# Hent Danmarks Statistiks officielle indikator
# --------------------------------------------------

# Find indikatorens navn i metadata
DST_sp <- forv1_meta$values$INDIKATOR$text[
  grepl(
    "Forbrugertillidsindikatoren",
    forv1_meta$values$INDIKATOR$text
  )
]

stopifnot(length(DST_sp) == 1)

# Hent månedsdata
DST_raw <- dst_get_data(
  table = "FORV1",
  query = list(
    INDIKATOR = DST_sp,
    Tid = "*"
  ),
  meta_data = forv1_meta,
  lang = "da",
  parse_dst_tid = FALSE
)

# Behold januar 2000 og frem
DST_raw <- DST_raw[DST_raw$TID >= "2000M01", ]

# Lav måneder til kvartaler
DST_maaned <- as.numeric(substr(DST_raw$TID, 6, 7))

DST_kvartal <- paste0(
  substr(DST_raw$TID, 1, 4),
  "K",
  ceiling(DST_maaned / 3)
)

# Beregn gennemsnit pr. kvartal
DST_kvt <- aggregate(
  DST_raw$value,
  by = list(TID = DST_kvartal),
  FUN = mean
)

names(DST_kvt)[2] <- "DST"


# --------------------------------------------------
# Match indikatorerne til samme kvartaler
# --------------------------------------------------

# Match DI og CCI til den eksisterende analyseperiode
raekker <- match(tid_stabil, forv1_kvt$TID)

DI_match <- DI[raekker]
CCI_match <- CCI[raekker]

# Match Danmarks Statistiks indikator
DST_match <- DST_kvt$DST[
  match(tid_stabil, DST_kvt$TID)
]

# Behold kun kvartaler med data for alle indikatorer
brug_sammenligning <- complete.cases(
  y_stabil,
  x_stabil,
  DI_match,
  CCI_match,
  DST_match
)

# Privatforbrugets årlige realvækst
y_sammenligning <- y_stabil[brug_sammenligning]

# Alle egne indikatorer
x_sammenligning <- x_stabil[
  brug_sammenligning, , drop = FALSE
]

# Kvartalerne i sammenligningen
tid_sammenligning <- tid_stabil[brug_sammenligning]

stopifnot(length(y_sammenligning) >= 3)


# --------------------------------------------------
# Find egen indikator med højest R²
# --------------------------------------------------

# Beregn R² for alle egne indikatorer
# på den fælles analyseperiode
R2_sammenligning <- as.numeric(
  cor(y_sammenligning, x_sammenligning)
)^2

# Find indikatoren med højest R²
bedste_nr <- which.max(R2_sammenligning)

# Find spørgsmålene i indikatoren
bedste_sp <- kombinationsnavne[bedste_nr]

# Vis den valgte kombination
bedste_sp


# --------------------------------------------------
# Saml de fire indikatorer
# --------------------------------------------------

indikatorer <- cbind(
  DI = DI_match[brug_sammenligning],
  CCI = CCI_match[brug_sammenligning],
  DST = DST_match[brug_sammenligning],
  Egen = x_sammenligning[, bedste_nr]
)

# Beregn korrelation med privatforbrugets realvækst
korrelation <- as.numeric(
  cor(y_sammenligning, indikatorer)
)

# Opret sammenligningstabel
sammenligning <- data.frame(
  Indikator = c(
    "DI",
    "CCI",
    "DST",
    "Egen indikator med højest R²"
  ),
  Spoergsmaal = c(
    paste(DI_sp, collapse = " + "),
    paste(CCI_sp, collapse = " + "),
    "F2 + F3 + F4 + F5 + F9",
    bedste_sp
  ),
  R2 = korrelation^2,
  Korrelation = korrelation
)


# Sortér efter højest R²
sammenligning <- sammenligning[
  order(sammenligning$R2, decreasing = TRUE),
]

rownames(sammenligning) <- NULL

# Vis resultaterne
View(sammenligning)


# --------------------------------------------------
# Graf: Sammenligning af de fire indikatorer
# --------------------------------------------------

# Indikatoren med højest R² skal stå øverst
vis <- nrow(sammenligning):1

# R² i procent
r2_procent <- sammenligning$R2[vis] * 100

# Navne på indikatorerne
etiketter <- sammenligning$Indikator[vis]

# R² med dansk decimalkomma og procenttegn
r2_tekst <- paste0(
  formatC(
    r2_procent,
    format = "f",
    digits = 1,
    decimal.mark = ","
  ),
  " %"
)

# Korrelation med dansk decimalkomma
kor_tekst <- paste0(
  "r = ",
  formatC(
    sammenligning$Korrelation[vis],
    format = "f",
    digits = 3,
    decimal.mark = ","
  )
)

# Fremhæv jeres egen indikator
farver <- ifelse(
  etiketter == "Egen indikator med højest R²",
  "steelblue",
  "grey75"
)

# Find analyseperioden
periode <- paste(
  min(tid_sammenligning),
  max(tid_sammenligning),
  sep = "–"
)

# Gem grafikindstillinger
gamle_indstillinger <- par(no.readonly = TRUE)

# Giv plads til navne, titel og noter
par(mar = c(9, 16, 5, 2))

# Tegn søjlerne
placeringer <- barplot(
  r2_procent,
  horiz = TRUE,
  names.arg = etiketter,
  col = farver,
  border = NA,
  xlim = c(0, 125),
  axes = FALSE,
  xlab = "Forklaret variation i privatforbrugets årlige realvækst (R²)",
  cex.lab = 0.8,
  las = 1,
  cex.names = 0.8
)

# Procenttegn på aksen
axis(
  side = 1,
  at = seq(0, 100, 20),
  labels = paste0(seq(0, 100, 20), " %"),
  cex.axis = 0.8
)

# Vis R² ved søjlernes ender
text(
  x = r2_procent,
  y = placeringer,
  labels = r2_tekst,
  pos = 4,
  cex = 0.85
)

# Vis korrelation i en separat kolonne
text(
  x = rep(105, nrow(sammenligning)),
  y = placeringer,
  labels = kor_tekst,
  adj = 0,
  cex = 0.8
)

# Titel
mtext(
  "Fire indikatorers sammenhæng med privatforbrugets realvækst",
  side = 3,
  line = 2,
  font = 2,
  cex = 0.95
)

# Undertitel
mtext(
  "Sorteret efter R² på samme fælles analyseperiode",
  side = 3,
  line = 0.5,
  cex = 0.8
)

# Analyseperiode
mtext(
  paste0("Analyseperiode: ", periode),
  side = 1, line = 4, adj = 0, cex = 0.7
)

# Spørgsmålene i jeres egen indikator
mtext(
  paste0("Egen indikator: ", bedste_sp, "."),
  side = 1, line = 5, adj = 0, cex = 0.7
)


# Spørgsmålene i DI, CCI og DST på én linje
mtext(
  "DI: F2, F4, F9 og F10.  CCI: F2, F3, F9 og F10.  DST: F2, F3, F4, F5 og F9.",
  side = 1, line = 6, adj = 0, cex = 0.65
)

# Kilde
mtext(
  "Kilde: Danmarks Statistik, FORV1 og NKH1; DI (2016); Jonsson og Lindén (2009);",
  side = 1, line = 7, adj = 0, cex = 0.65
)


# Gendan grafikindstillinger
par(gamle_indstillinger)



#--------------------------------------
# Opgave 1.4 – Prognose for realvækst i privatforbruget i 2026K3 og 2026K4
#--------------------------------------

# 1. Vælg det bedste indikatorsæt fra opgave 1.3
bedste_spoergsmaal <- c("F4", "F7", "F9", "F10", "F12", "F13")

# 2. Udvælg indikatorerne fra FORV1
indikator_data <- forv1_kvt %>%
  select(TID, all_of(bedste_spoergsmaal)) %>%
  arrange(TID)

# 3. Dan den samlede indikator som gennemsnittet af de seks spørgsmål
indikator_data <- indikator_data %>%
  mutate(indikator = rowMeans(
    across(all_of(bedste_spoergsmaal)),
    na.rm = FALSE
  ))

# 4. Beregn realvæksten i privatforbruget fra NKH1
#    Brug de nyeste data: ingen fast slutdato
forbrugMeta <- dst_meta("NKH1")

queryForbrug <- list(
  "P.31 Husholdningernes forbrugsudgifter",
  "2020-priser, kædede værdier",
  "Sæsonkorrigeret",
  "*"
)

names(queryForbrug) <- forbrugMeta$variables$id

forbrugRaw <- dst_get_data(
  "NKH1",
  query = queryForbrug,
  parse_dst_tid = FALSE
)

forbrugRaw <- forbrugRaw %>%
  filter(TID >= "1999K1") %>%
  arrange(TID)

forbrug_vaekst <- forbrugRaw %>%
  mutate(
    Realvaekst = (value / lag(value, 4) - 1) * 100
  ) %>%
  select(TID, Realvaekst)

# 5. Sammenkobl indikator og realvækst på kvartal
reg_data <- indikator_data %>%
  inner_join(forbrug_vaekst, by = "TID") %>%
  filter(!is.na(indikator), !is.na(Realvaekst))

# 6. Estimér modellen med det valgte indikatorsæt
model_14 <- lm(Realvaekst ~ indikator, data = reg_data)

summary(model_14)

# 7. Lav prognose for 2026K3 og 2026K4
prognose_data <- indikator_data %>%
  filter(TID %in% c("2026K3", "2026K4"))

# Tjek hvilke kvartaler der faktisk er tilgængelige
print(prognose_data)

if (nrow(prognose_data) > 0) {
  prognoser <- predict(model_14, newdata = prognose_data,
                       interval = "prediction")
  
  resultat <- cbind(
    prognose_data %>% select(TID, indikator),
    as.data.frame(prognoser)
  )
  
  print(resultat)
} else {
  message("Ingen indikator-data fundet for 2026K3 eller 2026K4.")
}

#Graf 1:Sammenhængen mellem forbrugertillid og realvækst
graf1 <- ggplot(reg_data, aes(x = indikator, y = Realvaekst)) +
  geom_point(alpha = 0.65, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Forbrugertillid og realvækst i privatforbruget",
    subtitle = "Lineær regression med 95 % konfidensinterval",
    x = "Sammensat forbrugertillidsindikator",
    y = "Årlig realvækst i privatforbruget (%)",
    caption = paste(
      "Kilde: Danmarks Statistik, FORV1 og NKH1.",
      "Egne beregninger."
    )
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(
      size = 9, hjust = 0, color = "grey30"
    ),
    plot.caption.position = "plot"
  )

print(graf1)

#graf 2: Prognoser for 2026K3 og 2026K4
resultat$TID <- factor(
  resultat$TID,
  levels = c("2026K3", "2026K4")
)

graf2 <- ggplot(resultat, aes(x = TID, y = fit)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_errorbar(
    aes(ymin = lwr, ymax = upr),
    width = 0.12,
    linewidth = 0.8
  ) +
  geom_point(size = 3) +
  
  # Placer tallene til højre for hvert punkt
  geom_text(
    aes(
      label = sprintf("%.2f %%", fit),
      x = as.numeric(TID) + 0.10
    ),
    hjust = 0,
    size = 4.5,
    fontface = "bold"
  ) +
  
  scale_x_discrete(
    expand = expansion(add = c(0.5, 1.0))
  ) +
  
  scale_y_continuous(
    limits = c(-3.5, 6),
    breaks = seq(-2, 6, by = 2)
  ) +
  
  labs(
    title = "Prognose for realvæksten i privatforbruget",
    subtitle = "2026K3 og 2026K4 med 95 % prædiktionsintervaller",
    x = "Kvartal",
    y = "Forudsagt årlig realvækst (%)",
    caption = paste0(
      "Kilde: Danmarks Statistik, tabel FORV1 (forbrugertillid) ",
      "og tabel NKH1 (privatforbrug).\n",
      "Egne beregninger på baggrund af en lineær regressionsmodel.\n",
      "Anm.: De lodrette linjer viser 95 % prædiktionsintervaller."
    )
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_minimal() +
  theme(
    plot.caption = element_text(
      size = 9,
      hjust = 0,
      color = "grey30",
      lineheight = 1.2
    ),
    plot.caption.position = "plot",
    plot.margin = margin(
      t = 15, r = 40, b = 20, l = 15
    )
  )

print(graf2)

# --------------------------------------------------
# Opgave 1.5: Den bedste mikroøkonomiske indikator
# --------------------------------------------------

# Behold kun mikroøkonomiske spørgsmål
mikro_sp <- c("F2", "F3", "F9", "F10", "F12", "F13")

# Brug kvartalsdata fra den tidligere kode
# Makrospørgsmålene F4-F8 indgår ikke
mikro_kvt <- forv1_kvt[, c("TID", mikro_sp)]

View(mikro_kvt)


# --------------------------------------------------
# For 1 spørgsmål: 6 kombinationer
# --------------------------------------------------

mikro_comb1 <- matrix(
  NA_real_,
  nrow = nrow(mikro_kvt),
  ncol = 6
)

for (i in 1:nrow(mikro_kvt)) {
  mikro_comb1[i, ] <- combn(
    as.numeric(mikro_kvt[i, -1]),
    1,
    FUN = mean
  )
}




# --------------------------------------------------
#Opgave 2
# --------------------------------------------------

#Pakker til opgave 2
library(dkstat)
library(tidyr)
library(dplyr)
library(pls)


# --------------------------------------------------
#Opgave 2.1 + 2.2
# --------------------------------------------------

# Saml realvækst og spørgsmål i ét datasæt
data.ml <- data.frame(
  Realvaekst = y,
  forv1_kvt[, c("F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F12","F13")]
)

# Fjern kvartaler med manglende værdier
data.ml <- data.ml[complete.cases(data.ml), ]

# PCR-model
pcr.fit <- pcr(
  Realvaekst ~ F2 + F3 + F4 + F5 + F6 + F7 + F8 + F9 + F10 + F12 + F13,
  data = data.ml,
  scale = TRUE,
  validation = "CV"
)

# Vis resultaterne
summary(pcr.fit)

loadings.pcr.fit <- pcr.fit$loadings

w.indicators.1 <- loadings.pcr.fit[, 1]^2

sum(w.indicators.1)

w.indicators.1

# Kvadrerede loadings for første komponent
View(data.frame(
  Bidrag = w.indicators.1,
  row.names = rownames(pcr.fit$loadings)
))

# Kvadrerede loadings for alle komponenter
View(pcr.fit$loadings^2)

# Kvadrerede loadings med spørgsmålenes navne
bidrag <- data.frame(
  Spoergsmaal = rownames(pcr.fit$loadings),
  as.matrix(pcr.fit$loadings)^2,
  row.names = NULL
)

View(bidrag)



# --------------------------------------------------
# Graf: Validering af PCA-regression
# --------------------------------------------------

# Hent cross-valideret MSEP
msep <- as.numeric(MSEP(pcr.fit, estimate = "CV")$val)

# Find antal komponenter med laveste MSEP
bedste <- which.min(msep) - 1

# Find analyseperioden
periode <- paste(
  min(tid_stabil),
  max(tid_stabil),
  sep = "–"
)

# Gem grafikindstillinger
gamle_indstillinger <- par(no.readonly = TRUE)
par(mar = c(8, 5, 6, 2))

# Tegn grafen
plot(
  0:11, msep,
  type = "o",
  pch = 16,
  col = "grey60",
  lwd = 2,
  xlab = "Antal hovedkomponenter",
  ylab = "Cross-valideret MSEP",
  main = "PCA: Laveste forudsigelsesfejl ved 9 komponenter",
  ylim = c(3.5, max(msep) + 0.5),
  xaxt = "n",
  las = 1
)

axis(1, at = 0:11)

# Fremhæv bedste model
points(
  bedste, msep[bedste + 1],
  pch = 19,
  col = "steelblue",
  cex = 1.8
)

# Vis teksten over den grå kurve
text(
  x = bedste,
  y = msep[bedste + 1] + 0.25,
  labels = paste0(
    "Laveste MSEP: ",
    formatC(
      msep[bedste + 1],
      digits = 2,
      format = "f",
      decimal.mark = ","
    )
  ),
  col = "steelblue4",
  cex = 0.95,
  font = 2,
  adj = 0.5
)

# Undertitel
mtext(
  "10-fold cross-validation | 11 spørgsmål fra FORV1",
  side = 3,
  line = 0.5,
  cex = 0.8
)

# Analyseperiode
mtext(
  paste0("Analyseperiode: ", periode),
  side = 1,
  line = 4,
  adj = 0,
  cex = 0.75
)

# Metodenote
mtext(
  "Lavere MSEP angiver mindre forudsigelsesfejl.",
  side = 1,
  line = 5,
  adj = 0,
  cex = 0.75
)

# Kilde
mtext(
  "Kilde: Danmarks Statistik, FORV1 og NKH1; egne beregninger.",
  side = 1,
  line = 6,
  adj = 0,
  cex = 0.75
)

# Gendan grafikindstillinger
par(gamle_indstillinger)

# --------------------------------------------------
# Opgave 2.3 Forudsig privatforbrugets realvækst
# --------------------------------------------------

# Hent nyeste forbrugerforventninger fra FORV1
forv_ny <- dst_get_data(
  table = "FORV1",
  query = list(
    INDIKATOR = spoergsmaal,
    Tid = "*"
  ),
  meta_data = forv1_meta,
  lang = "da",
  parse_dst_tid = FALSE
)

# Behold januar 2000 og frem
forv_ny <- forv_ny[forv_ny$TID >= "2000M01", ]

# Lav spørgsmålene til kolonner
forv_ny <- pivot_wider(
  forv_ny,
  id_cols = TID,
  names_from = INDIKATOR,
  values_from = value
)

forv_ny <- as.data.frame(forv_ny)

# Behold kun F-koderne
names(forv_ny)[-1] <- sub(" .*", "", names(forv_ny)[-1])

# Sortér spørgsmålene
numre_ny <- as.numeric(sub("^F", "", names(forv_ny)[-1]))
forv_ny <- forv_ny[, c(1, order(numre_ny) + 1)]

# Sortér månederne
forv_ny <- forv_ny[order(forv_ny$TID), ]

# Omdan måneder til kvartaler
maaned_ny <- as.numeric(substr(forv_ny$TID, 6, 7))

kvartal_ny <- paste0(
  substr(forv_ny$TID, 1, 4),
  "K",
  ceiling(maaned_ny / 3)
)

# Find kvartaler med tre måneder
antal_maaneder <- table(kvartal_ny)

fulde_kvartaler <- names(antal_maaneder)[
  antal_maaneder == 3
]

# Behold kun komplette kvartaler
forv_ny <- forv_ny[
  kvartal_ny %in% fulde_kvartaler,
]

kvartal_ny <- kvartal_ny[
  kvartal_ny %in% fulde_kvartaler
]

# Beregn kvartalsgennemsnit
forv_kvt_ny <- aggregate(
  forv_ny[, -1],
  by = list(TID = kvartal_ny),
  FUN = mean
)

# Vend fortegn på F6 og F8
forv_kvt_ny[, c("F6", "F8")] <-
  -forv_kvt_ny[, c("F6", "F8")]

# Find kvartaler uden faktisk realvækst
mangler_forbrug <- !forv_kvt_ny$TID %in%
  forbrug_vaekst$TID[
    !is.na(forbrug_vaekst$Realvaekst)
  ]

# Behold kun kvartaler uden faktisk realvækst
prognose_data <- forv_kvt_ny[mangler_forbrug, ]

# Kontrollér at der findes en prognoseperiode
stopifnot(nrow(prognose_data) > 0)

# Find seneste komplette prognosekvartal
seneste_kvartal <- max(prognose_data$TID)

# Hent de 11 spørgsmål
nye_data <- prognose_data[
  prognose_data$TID == seneste_kvartal,
  names(data.ml)[-1],
  drop = FALSE
]

# Kontrollér at alle spørgsmål har værdier
stopifnot(!anyNA(nye_data))

# Forudsig med eksisterende PCR-model
forudsigelse_PCA <- predict(
  pcr.fit,
  newdata = nye_data,
  ncomp = 9
)

# Saml resultatet
resultat_PCA <- data.frame(
  Kvartal = seneste_kvartal,
  Forudsagt_realvaekst = round(
    as.numeric(forudsigelse_PCA), 2
  )
)

# Vis prognosen
resultat_PCA


# --------------------------------------------------
# Graf: Faktisk realvækst og PCA-prognose
# --------------------------------------------------

# Faktisk realvækst
graf_data <- forbrug_vaekst[
  !is.na(forbrug_vaekst$Realvaekst),
]

# Vis de seneste 20 kvartaler
graf_data <- tail(graf_data, 20)

# Prognosen
prognose <- resultat_PCA$Forudsagt_realvaekst
prognose_kvartal <- resultat_PCA$Kvartal

# Y-akse med plads til alle værdier
y_graenser <- range(
  c(graf_data$Realvaekst, prognose)
) + c(-1, 1)

# Y-akse med intervaller på 2 procentpoint
y_tal <- seq(
  floor(y_graenser[1] / 2) * 2,
  ceiling(y_graenser[2] / 2) * 2,
  by = 2
)

# Tilpas grafens grænser
y_graenser <- range(y_tal)

# Gem grafikindstillinger
gamle_indstillinger <- par(no.readonly = TRUE)

# Plads til overskrifter, signaturforklaring og kilde
par(mar = c(8, 5, 8, 2))

# Tegn faktisk realvækst
plot(
  seq_len(nrow(graf_data)),
  graf_data$Realvaekst,
  type = "o",
  pch = 16,
  col = "grey50",
  xlim = c(1, nrow(graf_data) + 1.5),
  ylim = y_graenser,
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = "Årlig realvækst (%)",
  main = ""
)

# Y-akse med tal for hver 2 procentpoint
axis(
  side = 2,
  at = y_tal,
  labels = paste0(y_tal, " %"),
  las = 1,
  cex.axis = 0.85
)

# Vandret nul-linje
abline(
  h = 0,
  col = "grey65",
  lty = 2,
  lwd = 1.5
)

# Kvartaler på X-aksen
axis(
  side = 1,
  at = seq_len(nrow(graf_data) + 1),
  labels = c(graf_data$TID, prognose_kvartal),
  las = 2,
  cex.axis = 0.7
)

# Forbind seneste faktiske observation med prognosen
lines(
  c(nrow(graf_data), nrow(graf_data) + 1),
  c(tail(graf_data$Realvaekst, 1), prognose),
  col = "steelblue",
  lty = 2,
  lwd = 2
)

# Fremhæv prognosen
points(
  nrow(graf_data) + 1,
  prognose,
  col = "steelblue",
  pch = 19,
  cex = 1.5
)

# Vis prognoseværdien
text(
  nrow(graf_data) + 1,
  prognose,
  labels = paste0(
    formatC(
      prognose,
      digits = 2,
      format = "f",
      decimal.mark = ","
    ),
    " %"
  ),
  pos = 3,
  col = "steelblue",
  font = 2,
  xpd = NA
)

# Konkluderende overskrift
mtext(
  "PCA-modellen peger på fortsat vækst i privatforbruget",
  side = 3,
  line = 5.5,
  font = 2,
  cex = 1.1
)

# Underoverskrift
mtext(
  paste0(
    "Prognosen på ",
    formatC(
      prognose,
      format = "f",
      digits = 2,
      decimal.mark = ","
    ),
    " % ligger tæt på niveauet i de seneste kvartaler"
  ),
  side = 3,
  line = 3.7,
  cex = 0.85
)

# Signaturforklaring over grafområdet
legend(
  "top",
  inset = c(0, -0.12),
  legend = c("Faktisk realvækst", "PCA-prognose"),
  col = c("grey50", "steelblue"),
  lty = c(1, 2),
  pch = c(16, 19),
  horiz = TRUE,
  bty = "n",
  xpd = NA,
  cex = 0.85
)

# Kilde
mtext(
  "Kilde: Danmarks Statistik, NKH1 og FORV1; egne beregninger.",
  side = 1,
  line = 6,
  adj = 0,
  cex = 0.75
)

# Gendan grafikindstillinger
par(gamle_indstillinger)


# --------------------------------------------------
# Opgave 2.4 - Forudsigelser fra DI og Nationalbanken
# --------------------------------------------------

# --------------------------------------------------
# Del 1 - DI's prognose for 2016
# --------------------------------------------------

# Hent privatforbruget for 2015 og 2016
forbrug_2015 <- forbrugRaw$value[
  substr(forbrugRaw$TID, 1, 4) == "2015"
]

forbrug_2016 <- forbrugRaw$value[
  substr(forbrugRaw$TID, 1, 4) == "2016"
]

# Kontrollér at begge år indeholder fire kvartaler
stopifnot(
  length(forbrug_2015) == 4,
  length(forbrug_2016) == 4
)

# Beregn faktisk årlig realvækst i 2016
faktisk_2016 <- (
  sum(forbrug_2016) / sum(forbrug_2015) - 1
) * 100

# Vis faktisk realvækst i 2016
round(faktisk_2016, 2)


# --------------------------------------------------
# Del 2 - DI og Nationalbankens prognoser for 2025
# --------------------------------------------------

# Hent privatforbruget for 2024 og 2025
forbrug_2024 <- forbrugRaw$value[
  substr(forbrugRaw$TID, 1, 4) == "2024"
]

forbrug_2025 <- forbrugRaw$value[
  substr(forbrugRaw$TID, 1, 4) == "2025"
]

# Kontrollér at begge år indeholder fire kvartaler
stopifnot(
  length(forbrug_2024) == 4,
  length(forbrug_2025) == 4
)

# Beregn faktisk årlig realvækst i 2025
faktisk_2025 <- (
  sum(forbrug_2025) / sum(forbrug_2024) - 1
) * 100

# DI's prognose fra maj 2025
prognose_DI_2025 <- 2.6

# Nationalbankens prognose fra september 2025
prognose_NB_2025 <- 2.2

# Sammenlign prognoserne med faktisk realvækst
sammenligning_2025 <- data.frame(
  Kilde = c("Dansk Industri", "Nationalbanken"),
  Prognose = c(prognose_DI_2025, prognose_NB_2025),
  Faktisk = round(faktisk_2025, 2),
  Prognosefejl = round(
    faktisk_2025 - c(prognose_DI_2025, prognose_NB_2025),
    2
  )
)

# Vis den samlede sammenligning
sammenligning_2025


# ============================================================
# OPGAVE 3.1 - MACHINE LEARNING
# Forudsig om forbruget i K4 2026 er større end K4 2025
# ============================================================


# ------------------------------------------------------------
# 1. Behold kun K4 fra privatforbruget
# ------------------------------------------------------------

forbrug_k4 <- forbrugRaw[
  grepl("K4$", forbrugRaw$TID),
]


# Lav år
forbrug_k4$Aar <- as.numeric(
  substr(forbrug_k4$TID, 1, 4)
)


# Sortér efter år
forbrug_k4 <- forbrug_k4[
  order(forbrug_k4$Aar),
]


# ------------------------------------------------------------
# 2. Lav Y-variablen
#
# 1 = K4 er større end K4 året før
# 0 = K4 er ikke større end K4 året før
# ------------------------------------------------------------

forbrug_k4$Stoerre_end_sidste_aar <- c(
  NA,
  ifelse(
    diff(forbrug_k4$value) > 0,
    1,
    0
  )
)


View(forbrug_k4)


# ------------------------------------------------------------
# 3. Lav år og måned på DST's officielle
# forbrugertillidsindikator
# ------------------------------------------------------------

DST_raw$Aar <- as.numeric(
  substr(DST_raw$TID, 1, 4)
)

DST_raw$Maaned <- as.numeric(
  substr(DST_raw$TID, 6, 7)
)


# ------------------------------------------------------------
# 4. Behold K3
# juli, august og september
# ------------------------------------------------------------

FTI_k3 <- DST_raw[
  DST_raw$Maaned %in% c(7, 8, 9),
]


# ------------------------------------------------------------
# 5. Beregn gennemsnitlig forbrugertillid i K3
# ------------------------------------------------------------

FTI_k3_aar <- aggregate(
  FTI_k3$value,
  by = list(
    Aar = FTI_k3$Aar
  ),
  FUN = mean
)


names(FTI_k3_aar)[2] <- "FTI_K3"


View(FTI_k3_aar)


# ------------------------------------------------------------
# 6. Saml X og Y i ét datasæt
# ------------------------------------------------------------

data_ml <- merge(
  forbrug_k4[
    ,
    c(
      "Aar",
      "value",
      "Stoerre_end_sidste_aar"
    )
  ],
  FTI_k3_aar,
  by = "Aar"
)


names(data_ml)[2] <- "Forbrug_K4"


# ------------------------------------------------------------
# 7. Fjern manglende værdier
# ------------------------------------------------------------

data_ml <- data_ml[
  complete.cases(
    data_ml$Stoerre_end_sidste_aar,
    data_ml$FTI_K3
  ),
]


View(data_ml)


# ------------------------------------------------------------
# 8. Se antal 0 og 1
# ------------------------------------------------------------

table(
  data_ml$Stoerre_end_sidste_aar
)


# ------------------------------------------------------------
# 9. Logistisk regression
#
# X = FTI i K3
# Y = om K4 er større end året før
# ------------------------------------------------------------

model_ml <- glm(
  Stoerre_end_sidste_aar ~ FTI_K3,
  data = data_ml,
  family = binomial
)


summary(model_ml)


# ------------------------------------------------------------
# 10. Beregn sandsynligheder på historiske data
# ------------------------------------------------------------

data_ml$Sandsynlighed <- predict(
  model_ml,
  newdata = data_ml,
  type = "response"
)


# ------------------------------------------------------------
# 11. Lav klassifikation
#
# >= 50 % = 1
# < 50 % = 0
# ------------------------------------------------------------

data_ml$Forudsigelse <- ifelse(
  data_ml$Sandsynlighed >= 0.5,
  1,
  0
)


View(data_ml)


# ------------------------------------------------------------
# 12. Confusion matrix
# ------------------------------------------------------------

table(
  Faktisk = data_ml$Stoerre_end_sidste_aar,
  Forudsagt = data_ml$Forudsigelse
)


# ------------------------------------------------------------
# 13. Accuracy
# ------------------------------------------------------------

accuracy <- mean(
  data_ml$Stoerre_end_sidste_aar ==
    data_ml$Forudsigelse
)

accuracy


# ============================================================
# 14. FORUDSIG K4 2026
# ============================================================


# Find FTI-gennemsnittet for K3 2026
FTI_2026 <- FTI_k3_aar$FTI_K3[
  FTI_k3_aar$Aar == 2026
]


# Nyt datasæt til prediction
ny_2026 <- data.frame(
  FTI_K3 = FTI_2026
)


# Sandsynligheden for at K4 2026
# bliver større end K4 2025
sandsynlighed_2026 <- predict(
  model_ml,
  newdata = ny_2026,
  type = "response"
)


sandsynlighed_2026


# ------------------------------------------------------------
# 15. Endelig prediction
# ------------------------------------------------------------

forudsigelse_2026 <- ifelse(
  sandsynlighed_2026 >= 0.5,
  1,
  0
)


forudsigelse_2026


# ============================================================
# OPGAVE 3.2 - VALIDERING AF MODELLEN
# Walk-forward validation fra 2010 til 2025
# ============================================================

# 16 år skal forudsiges
test_aar <- 2010:2025

# Gem resultater
resultater <- data.frame(
  Aar = integer(),
  Faktisk = numeric(),
  Sandsynlighed = numeric(),
  Forudsagt = numeric()
)

# ------------------------------------------------------------
# 1. Test modellen år for år
# ------------------------------------------------------------

for (aar in test_aar) {
  
  # Kun tidligere år bruges til træning
  traening <- data_ml[data_ml$Aar < aar, ]
  
  # Det år vi vil forudsige
  test <- data_ml[data_ml$Aar == aar, ]
  
  if (nrow(traening) == 0 || nrow(test) != 1) {
    stop(paste("Manglende data for", aar))
  }
  
  # Kræver både 0 og 1 i træningsdata
  if (length(unique(
    traening$Stoerre_end_sidste_aar
  )) < 2) {
    stop(paste("Kun én Y-klasse før", aar))
  }
  
  # Træn logistisk regression
  model <- glm(
    Stoerre_end_sidste_aar ~ FTI_K3,
    data = traening,
    family = binomial
  )
  
  # Forudsig det nye år
  p <- predict(
    model,
    newdata = test,
    type = "response"
  )
  
  # Klassifikation
  pred <- ifelse(p >= 0.5, 1, 0)
  
  # Gem resultat
  resultater <- rbind(
    resultater,
    data.frame(
      Aar = aar,
      Faktisk = test$Stoerre_end_sidste_aar,
      Sandsynlighed = as.numeric(p),
      Forudsagt = as.numeric(pred)
    )
  )
}

View(resultater)


# ------------------------------------------------------------
# 2. Confusion matrix
# ------------------------------------------------------------

cm <- table(
  Faktisk = factor(resultater$Faktisk,
                   levels = c(0, 1)),
  Forudsagt = factor(resultater$Forudsagt,
                     levels = c(0, 1))
)

cm


# ------------------------------------------------------------
# 3. Accuracy
# ------------------------------------------------------------

accuracy_test <- mean(
  resultater$Faktisk == resultater$Forudsagt
)

accuracy_test


# ------------------------------------------------------------
# 4. Baseline - forudsig altid 1
# ------------------------------------------------------------

baseline_accuracy <- mean(
  resultater$Faktisk == 1
)

baseline_accuracy


# ------------------------------------------------------------
# 5. Sensitivity og specificity
# ------------------------------------------------------------

sensitivity <- cm["1", "1"] / sum(cm["1", ])

specificity <- cm["0", "0"] / sum(cm["0", ])

balanced_accuracy <- mean(
  c(sensitivity, specificity)
)


# ------------------------------------------------------------
# 6. Sammenlign resultater
# ------------------------------------------------------------

sammenligning <- data.frame(
  Model = c("Logistisk regression",
            "Baseline - altid ja"),
  Accuracy = round(
    100 * c(accuracy_test, baseline_accuracy),
    1
  )
)

print(sammenligning)

cat("Sensitivity:", round(sensitivity * 100, 1), "%\n")
cat("Specificity:", round(specificity * 100, 1), "%\n")
cat("Balanced accuracy:",
    round(balanced_accuracy * 100, 1), "%\n")


# ------------------------------------------------------------
# 7. Se hvilke år modellen ramte forkert
# ------------------------------------------------------------

resultater$Korrekt <- (
  resultater$Faktisk == resultater$Forudsagt
)

View(resultater)

resultater[resultater$Korrekt == FALSE, ]

# --------------------------------------------------
#Opgave 4
# --------------------------------------------------
# Opgave 4.1: Stabilitet af indikatoren fra opgave 1.2
# --------------------------------------------------

# Følg den samme indikator i alle delperioder
stabil_nr <- bedste_nr

# Kontrollér, hvilken indikator vi undersøger
kombinationsnavne[stabil_nr]

# Saml resultater fra de tre loops
stabil_R2 <- list(
  R2_slut[, stabil_nr],
  R2_start[, stabil_nr],
  R2_begge[, stabil_nr]
)

stabil_rang <- list(
  rang_slut[, stabil_nr],
  rang_start[, stabil_nr],
  rang_begge[, stabil_nr]
)

# Opret oversigt
stabil_oversigt <- data.frame(
  Loop = c(
    "Fjern fra slutningen",
    "Fjern fra begyndelsen",
    "Fjern fra begge ender"
  ),
  Laveste_R2 = NA_real_,
  Median_R2 = NA_real_,
  Hoejeste_R2 = NA_real_,
  Bedste_rang = NA_real_,
  Median_rang = NA_real_,
  Daarligste_rang = NA_real_
)

for (i in 1:3) {
  
  stabil_oversigt$Laveste_R2[i] <- min(stabil_R2[[i]])
  stabil_oversigt$Median_R2[i] <- median(stabil_R2[[i]])
  stabil_oversigt$Hoejeste_R2[i] <- max(stabil_R2[[i]])
  
  stabil_oversigt$Bedste_rang[i] <- min(stabil_rang[[i]])
  stabil_oversigt$Median_rang[i] <- median(stabil_rang[[i]])
  stabil_oversigt$Daarligste_rang[i] <- max(stabil_rang[[i]])
}

View(stabil_oversigt)

# --------------------------------------------------
# Supplement: Kortsigtede udsving – egen indikator og DI
# --------------------------------------------------

# Fasthold indikatoren fra opgave 1.2
stabil_nr <- bedste_nr

# Find DI's kombination: F2 + F4 + F9 + F10
di_nr <- which(
  gsub(" ", "", kombinationsnavne) == "F2+F4+F9+F10"
)

# Kontrollér, at DI findes én gang
stopifnot(length(di_nr) == 1)

# Saml de to indikatorer på de samme kvartaler
# Brug kun kvartaler med gyldig realvækst,
# præcis som i punktdiagrammet
gyldige <- which(is.finite(y))

# Kontrollér, at kvartalerne er sammenhængende
stopifnot(
  length(gyldige) >= 3,
  all(diff(gyldige) == 1)
)

udsving_data <- comb_alle[
  gyldige,
  c(stabil_nr, di_nr),
  drop = FALSE
]

# Kontrollér, at der ikke er manglende eller uendelige værdier
stopifnot(all(is.finite(udsving_data)))

# Standardisér hver indikator
udsving_standard <- scale(udsving_data)

# Beregn standardafvigelsen af kvartalsændringerne
udsving <- numeric(2)

for (i in 1:2) {
  udsving[i] <- sd(diff(udsving_standard[, i]))
}

# Saml resultaterne
udsving_oversigt <- data.frame(
  Indikator = c("Egen indikator", "DI"),
  Kortsigtede_udsving = udsving
)

View(udsving_oversigt)

# Positiv værdi: egen indikator har mindre udsving end DI
# Negativ værdi: egen indikator har større udsving end DI
forskel_pct <- (1 - udsving[1] / udsving[2]) * 100

forskel_pct

# --------------------------------------------------
# Punktdiagram: Forklaringsgrad og kortsigtede udsving
# --------------------------------------------------

# 1. Find vores valgte indikator og DI
stabil_nr <- bedste_nr

di_nr <- which(
  gsub(" ", "", kombinationsnavne) == "F2+F4+F9+F10"
)

# 2. Kontrollér, at data passer sammen
stopifnot(
  length(di_nr) == 1,
  nrow(comb_alle) == length(y),
  nrow(forv1_kvt) == nrow(comb_alle),
  all(is.finite(comb_alle))
)

# 3. Udvælg kvartaler med gyldig realvækst
gyldige <- which(is.finite(y))

# Kvartalerne skal ligge i kronologisk rækkefølge.
# Kontrollér, at udvælgelsen ikke skaber huller.
stopifnot(
  length(gyldige) >= 3,
  all(diff(gyldige) == 1)
)

y_plot <- y[gyldige]
comb_plot <- comb_alle[gyldige, , drop = FALSE]

# Hent den konkrete analyseperiode
kvartaler_plot <- as.character(forv1_kvt$TID[gyldige])

periode <- paste(
  kvartaler_plot[1],
  "–",
  kvartaler_plot[length(kvartaler_plot)]
)

# 4. Standardisér hver indikator
indikator_standard <- scale(comb_plot)

# 5. Beregn udsving og R² for alle indikatorer
udsving_alle <- numeric(ncol(comb_plot))
R2_alle <- numeric(ncol(comb_plot))

for (i in 1:ncol(comb_plot)) {
  
  udsving_alle[i] <- sd(
    diff(indikator_standard[, i])
  )
  
  R2_alle[i] <- cor(comb_plot[, i], y_plot)^2
}

# 6. Oversigt med de opdaterede resultater
udsving_oversigt <- data.frame(
  Indikator = c("Egen indikator", "DI"),
  Kortsigtede_udsving = udsving_alle[c(stabil_nr, di_nr)],
  R2 = R2_alle[c(stabil_nr, di_nr)]
)

print(udsving_oversigt)

# Positiv værdi betyder større udsving for egen indikator
forskel_pct <- (
  udsving_alle[stabil_nr] / udsving_alle[di_nr] - 1
) * 100

print(forskel_pct)

# 7. Danske talformater til figuren
tal_dk <- function(x, decimaler = 1) {
  formatC(
    x,
    format = "f",
    digits = decimaler,
    decimal.mark = ","
  )
}

# Tekster ved de fremhævede punkter
egen_tekst <- paste0(
  "Egen indikator\nR²: ",
  tal_dk(R2_alle[stabil_nr] * 100),
  " % · Udsving: ",
  tal_dk(udsving_alle[stabil_nr], 3)
)

di_tekst <- paste0(
  "DI\nR²: ",
  tal_dk(R2_alle[di_nr] * 100),
  " % · Udsving: ",
  tal_dk(udsving_alle[di_nr], 3)
)

# 8. Gem grafikindstillinger
gamle_indstillinger <- par(no.readonly = TRUE)

# Ekstra plads til titel, periode, note og kilde
par(
  mar = c(9, 5, 5, 2),
  mgp = c(3, 1, 0)
)

# Akser
x_akse <- pretty(range(udsving_alle), n = 5)
y_top <- ceiling(max(R2_alle) * 100 / 10) * 10 + 10
y_akse <- seq(0, y_top, by = 10)

# 9. Opret diagrammet
plot(
  udsving_alle,
  R2_alle * 100,
  type = "n",
  xlim = range(x_akse),
  ylim = c(0, y_top),
  xlab = "Kortsigtede udsving",
  ylab = "Forklaringsgrad, R² (%)",
  main = "Forklaringsgrad og kortsigtede udsving",
  cex.main = 1.15,
  axes = FALSE,
  xaxs = "i",
  yaxs = "i"
)

# Konkret analyseperiode
mtext(
  paste0(
    "Analyseperiode: ", periode,
    " · ", length(gyldige), " kvartaler"
  ),
  side = 3,
  line = 0.6,
  cex = 0.85
)

# Diskrete hjælpelinjer
abline(
  v = x_akse,
  h = y_akse,
  col = "grey90",
  lty = 3
)

# Danske decimalkommaer på x-aksen
axis(
  1,
  at = x_akse,
  labels = tal_dk(x_akse),
  las = 1
)

axis(
  2,
  at = y_akse,
  labels = y_akse,
  las = 1
)

box(bty = "l")

# Øvrige indikatorer
oevrige <- setdiff(
  seq_len(ncol(comb_plot)),
  c(stabil_nr, di_nr)
)

points(
  udsving_alle[oevrige],
  R2_alle[oevrige] * 100,
  pch = 16,
  col = adjustcolor("grey50", alpha.f = 0.25),
  cex = 0.8
)

# Egen indikator
points(
  udsving_alle[stabil_nr],
  R2_alle[stabil_nr] * 100,
  pch = 16,
  col = "#0072B2",
  cex = 1.5
)

text(
  udsving_alle[stabil_nr],
  R2_alle[stabil_nr] * 100,
  labels = egen_tekst,
  pos = 3,
  offset = 0.8,
  col = "#0072B2",
  cex = 0.8
)

# DI
points(
  udsving_alle[di_nr],
  R2_alle[di_nr] * 100,
  pch = 17,
  col = "#D55E00",
  cex = 1.5
)

text(
  udsving_alle[di_nr],
  R2_alle[di_nr] * 100,
  labels = di_tekst,
  pos = 3,
  offset = 0.8,
  col = "#D55E00",
  cex = 0.8
)

# Signaturforklaring
legend(
  "topright",
  legend = c("Øvrige indikatorer", "Egen indikator", "DI"),
  col = c("grey65", "#0072B2", "#D55E00"),
  pch = c(16, 16, 17),
  bty = "n",
  cex = 0.8
)

# 10. Forklaring og kilde
mtext(
  paste0(
    "Note: Hvert punkt viser én af ",
    format(
      ncol(comb_plot),
      big.mark = ".",
      scientific = FALSE,
      trim = TRUE
    ),
    " indikatorer."
  ),
  side = 1, line = 4.3, adj = 0, cex = 0.7
)

mtext(
  "Udsving måles som standardafvigelsen af kvartalsændringerne i den standardiserede indikator.",
  side = 1, line = 5.3, adj = 0, cex = 0.7
)

mtext(
  "Lavere udsving betyder mere jævn udvikling. Målet skelner ikke mellem støj og reelle ændringer.",
  side = 1, line = 6.3, adj = 0, cex = 0.7
)

mtext(
  "Kilde: Danmarks Statistik, FORV1 og NKH1. Egne beregninger.",
  side = 1, line = 7, adj = 0, cex = 0.7
)

# Gendan grafikindstillinger
par(gamle_indstillinger)

# --------------------------------------------------
#Opgave 2
# --------------------------------------------------
#Opgave 2.1
# --------------------------------------------------

# Saml realvækst og spørgsmål i ét datasæt
data.ml <- data.frame(
  Realvaekst = y,
  forv1_kvt[, c("F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F12","F13")]
)

# Fjern kvartaler med manglende værdier
data.ml <- data.ml[complete.cases(data.ml), ]

# PCR-model
pcr.fit <- pcr(
  Realvaekst ~ F2 + F3 + F4 + F5 + F6 + F7 + F8 + F9 + F10 + F12 + F13,
  data = data.ml,
  scale = TRUE,
  validation = "CV"
)

# Vis resultaterne
summary(pcr.fit)

loadings.pcr.fit <- pcr.fit$loadings

w.indicators.1 <- loadings.pcr.fit[, 1]^2

sum(w.indicators.1)

w.indicators.1

# Kvadrerede loadings for første komponent
View(data.frame(
  Bidrag = w.indicators.1,
  row.names = rownames(pcr.fit$loadings)
))

# Kvadrerede loadings for alle komponenter
View(pcr.fit$loadings^2)

# Kvadrerede loadings med spørgsmålenes navne
bidrag <- data.frame(
  Spoergsmaal = rownames(pcr.fit$loadings),
  as.matrix(pcr.fit$loadings)^2,
  row.names = NULL
)

View(bidrag)

# --------------------------------------------------
#Opgave 5 
# --------------------------------------------------
# Opgave 5.2: Vind i november 2022
# --------------------------------------------------
# 1. Indlæs pakker

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# 2. Funktion til at hente data fra DMI

getdatafromdmi <- function(param, station, frd, tod) {
  
  # Sammensæt forespørgslen
  varurl <- paste0(
    "parameterId=", param,
    "&stationId=", station,
    "&datetime=", frd, "/", tod,
    "&limit=10000"
  )
  
  totalurl <- paste0(baseurl, varurl)
  
  print(totalurl)
  
  # Hent data fra DMI
  res <- GET(totalurl)
  
  # Stop, hvis forespørgslen mislykkes
  stop_for_status(res)
  
  # Læs svaret og omdan JSON til R-objekter
  resraw <- content(res, as = "text", encoding = "UTF-8")
  resdf <- fromJSON(resraw)
  
  # Udtræk måleværdi og tidspunkt
  obsdf <- resdf$features$properties
  winddf <- obsdf[, c("value", "observed")]
  
  # Omdan tidspunktet til dato og klokkeslæt
  winddf$observed <- as.POSIXct(
    winddf$observed,
    format = "%Y-%m-%dT%H:%M:%S",
    tz = "UTC"
  )
  
  # Sortér efter tidspunkt
  winddf <- arrange(winddf, observed)
  
  return(winddf)
}


# 3. Vælg parametre, stationer og periode

# Vindretning og vindhastighed den seneste time
windirparam <- "wind_dir_past1h"
windforceparam <- "wind_speed_past1h"

# Stationer
aarhsid <- "06074"
asid <- "06079"

# Adresse til DMI's observationer
baseurl <- paste0(
  "https://opendataapi.dmi.dk/v2/metObs/",
  "collections/observation/items?"
)

# Underviserens oprindelige periode
fd <- "2022-11-14T00:00:00Z"
td <- "2022-11-21T00:00:00Z"


# 4. Hent og saml data for Århus Syd

# Hent vindhastighed
windforcedf <- getdatafromdmi(
  windforceparam, aarhsid, fd, td
)

# Hent vindretning
winddirdf <- getdatafromdmi(
  windirparam, aarhsid, fd, td
)

# Navngiv målingerne før sammenkoblingen
names(windforcedf)[1] <- "force"
names(winddirdf)[1] <- "dir"

# Saml målingerne på deres fælles tidspunkt
totalwind <- merge(
  windforcedf,
  winddirdf,
  by = "observed",
  all = TRUE
)

# Tilføj stationsnavn
totalwind$Station <- "Århus Syd"

# 5. Hent og saml data for Anholt

windforcedfa <- getdatafromdmi(
  windforceparam, asid, fd, td
)

winddirdfa <- getdatafromdmi(
  windirparam, asid, fd, td
)

names(windforcedfa)[1] <- "force"
names(winddirdfa)[1] <- "dir"

totalwinda <- merge(
  windforcedfa,
  winddirdfa,
  by = "observed",
  all = TRUE
)

totalwinda$Station <- "Anholt"

# 6. Saml de to stationer

vind <- rbind(totalwind, totalwinda)

vind <- arrange(vind, Station, observed)

View(vind)

# 7. Fælles farver og grafisk opsætning

stationsfarver <- c(
  "Århus Syd" = "steelblue4",
  "Anholt" = "darkorange3"
)

graf_tema <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      size = 10,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      margin = margin(t = 12)
    ),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


# 8. Graf over vindhastigheden

graf_hastighed <- ggplot(
  vind,
  aes(
    x = observed,
    y = force,
    color = Station,
    linetype = Station
  )
) +
  geom_line(
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = stationsfarver
  ) +
  scale_linetype_manual(
    values = c(
      "Århus Syd" = "solid",
      "Anholt" = "dashed"
    )
  ) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%d/%m",
    timezone = "UTC"
  ) +
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  labs(
    title = "Vindhastigheden ved Århus Syd og Anholt",
    subtitle = paste0(
      "14. november kl. 00:00 til ",
      "21. november kl. 00:00, 2022 (UTC)"
    ),
    x = "Dato (UTC)",
    y = "Vindhastighed (m/s)",
    color = "Station",
    linetype = "Station",
    caption = paste0(
      "Note: Gennemsnitlig vindhastighed den seneste time, ",
      "ikke vindstød.\n",
      "Kilde: DMI, metObs, wind_speed_past1h; ",
      "egen fremstilling."
    )
  ) +
  graf_tema

print(graf_hastighed)


# 9. Klargør vindretningen

vind_retning <- vind[
  !is.na(vind$dir) &
    vind$dir >= 0 &
    vind$dir <= 360,
]


# 10. Graf over vindretningen

# Punkter bruges, fordi retningen er cirkulær:
# 359 grader og 1 grad ligger tæt på hinanden

graf_retning <- ggplot(
  vind_retning,
  aes(
    x = observed,
    y = dir,
    color = Station
  )
) +
  geom_point(
    size = 1.5,
    alpha = 0.8
  ) +
  facet_wrap(
    ~ Station,
    ncol = 1
  ) +
  scale_color_manual(
    values = stationsfarver
  ) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%d/%m",
    timezone = "UTC"
  ) +
  scale_y_continuous(
    limits = c(0, 360),
    breaks = seq(0, 360, 45),
    labels = c(
      "N", "NØ", "Ø", "SØ", "S",
      "SV", "V", "NV", "N"
    )
  ) +
  labs(
    title = "Vindretningen ved Århus Syd og Anholt",
    subtitle = paste0(
      "14. november kl. 00:00 til ",
      "21. november kl. 00:00, 2022 (UTC)"
    ),
    x = "Dato (UTC)",
    y = "Retningen, vinden kommer fra",
    caption = paste0(
      "Note: Hvert punkt viser den seneste times vindretning. ",
      "Kilde: DMI, metObs, wind_dir_past1h; ",
      "egen fremstilling."
    )
  ) +
  graf_tema +
  theme(
    legend.position = "none"
  )

print(graf_retning)

# --------------------------------------------------
# Opgave 5.2: Del 2. Vindhastighed i oktober 2023
# --------------------------------------------------
# 1. Vælg periode, parameter og stationer

# Perioden dækker dagene før, under og efter hændelsen
fd_okt <- "2023-10-18T00:00:00Z"
td_okt <- "2023-10-23T00:00:00Z"

# Gennemsnitlig vindhastighed den seneste time
windforceparam <- "wind_speed_past1h"

# Stations-ID'er
aarhsid <- "06074"
asid <- "06079"

# Adresse til DMI's observationer
baseurl <- paste0(
  "https://opendataapi.dmi.dk/v2/metObs/",
  "collections/observation/items?"
)


# 2. Hent vindhastighed for Århus Syd

aarhus_okt <- getdatafromdmi(
  windforceparam,
  aarhsid,
  fd_okt,
  td_okt
)

# Omdøb måleværdien og tilføj stationsnavn
names(aarhus_okt)[1] <- "force"

aarhus_okt$Station <- "Århus Syd"


# 3. Hent vindhastighed for Anholt

anholt_okt <- getdatafromdmi(
  windforceparam,
  asid,
  fd_okt,
  td_okt
)

names(anholt_okt)[1] <- "force"

anholt_okt$Station <- "Anholt"

# 4. Saml stationerne i ét datasæt

vind_okt <- rbind(aarhus_okt, anholt_okt)

# Sortér efter station og tidspunkt
vind_okt <- arrange(vind_okt, Station, observed)

View(vind_okt)


# 5. Tegn grafen med to linjer

graf_okt <- ggplot(
  vind_okt,
  aes(
    x = observed,
    y = force,
    color = Station
  )
) +
  
  # Én linje for hver station
  geom_line(
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  
  # Faste farver
  scale_color_manual(
    values = c(
      "Århus Syd" = "blue",
      "Anholt" = "red"
    ),
    breaks = c("Århus Syd", "Anholt")
  ) +
  
  # Vis datoer på x-aksen
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%d/%m",
    timezone = "UTC"
  ) +
  
  # Vindhastighedens akse starter ved nul
  scale_y_continuous(
    limits = c(0, NA)
  ) +
  
  # Titel, akser, analyseperiode og kilde
  labs(
    title = "Vindhastigheden ved Århus Syd og Anholt",
    subtitle = paste0(
      "Perioden omkring stormen i oktober 2023\n",
      "18. oktober kl. 00:00 til 23. oktober kl. 00:00 (UTC)"
    ),
    x = "Dato (UTC)",
    y = "Vindhastighed (m/s)",
    color = "Station",
    caption = paste0(
      "Note: Målingerne viser den seneste times ",
      "gennemsnitlige vindhastighed, ikke vindstød.\n",
      "Kilde: DMI, metObs, wind_speed_past1h; ",
      "egen fremstilling."
    )
  ) +
  
  # Enkel grafisk opsætning
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      size = 10,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      margin = margin(t = 12)
    ),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

# Vis grafen
print(graf_okt)


