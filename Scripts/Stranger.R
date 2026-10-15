library(stringr) # per manipolazione stringhe e funzioni pattern matching
library(readxl)  # lettura file excel
library(dplyr) # per manipolazione data frame
library(tidyr) # per manipolazione data frame
library(ggplot2) # per visualizzazioni grafiche
library(reshape2) # per manipolazione dati
library(tm) # per text mining
library(tidytext) # per text mining
library(udpipe) # per POS tagging(forse non utilizzata)
library(textclean) # per pulizia testi
library(topicmodels) # per topic modeling
library(ldatuning) # per topic modeling
library(quanteda) # per topic modeling

#leggiamo il file con le recensioni
dta <- read_excel("stranger_things.xlsx")

dta[, 1] <- NULL # eliminiamo la prima colonna che contiene indice recensioni

# Aggiunge un id per ogni recensione
dta = dta %>% mutate(id = 1:n())
head(dta)

# analizziamo le nostre variabili di contesto

#tasformiamo rating in numerica
dta$rating <- as.numeric(dta$rating)
summary(dta$rating)

#vediamo che ci sono 168 NA in rating, li sostituiamo con la mediana (valutazione pari a 7)

dta$rating[is.na(dta$rating)] <- median(dta$rating, na.rm = TRUE)
summary(dta$rating)

#controlliamo adesso upvote e downvote

table(dta$upvote)
#vediamo che ci sono i valori 1k, 1.1K, 1.2K, 1.4K,  1.8K e 2.6K, li tasformiamo in numerici

dta$upvote <- gsub("1K", "1000", dta$upvote)
dta$upvote <- gsub("1.1K", "1100", dta$upvote)
dta$upvote <- gsub("1.2K", "1200", dta$upvote)
dta$upvote <- gsub("1.4K", "1400", dta$upvote)
dta$upvote <- gsub("1.8K", "1800", dta$upvote)
dta$upvote <- gsub("2.6K", "2600", dta$upvote)

dta$upvote <- as.numeric(dta$upvote)

#facciamo la stessa cosa per i downvote

table(dta$downvote)
#vediamo che ci sono i valori 1.4K, 1.5K

dta$downvote <- gsub("1.4K", "1400", dta$downvote)
dta$downvote <- gsub("1.5K", "1500", dta$downvote)
dta$downvote <- as.numeric(dta$downvote)

#controlliamo le correlazioni tra le variabili numeriche

cor(dta[, c("rating", "upvote", "downvote", "episodio")])
# notiamo che c'è una correlazione negativa tra rating e upvote e tra rating e downvote , in entrambi i casi abbastanza bassa
# questo ci daa indicazione che le recensioni con voti più alti tendono a ricevere meno interazioni rispetto a quelle con voti bassi
# possiamo ipotizzare che le recensioni con voti bassi generano più discussioni tra gli utenti
# inoltre notiamo una correlazione positiva tra upvote e downvote, il che è logico in quanto le recensioni
# più popolari tendono a ricevere sia più upvote che downvote

### da fare in maniera più raffinata se c'è tempo
# per ciascun personaggio principale creiamo una variabile dicotomica che indica se il 
# personaggio è menzionato nella recensione
dta$eleven <- ifelse(str_detect(tolower(dta$recensione), "eleven|Jane|\b el \b"), 1, 0)
dta$hopper <- ifelse(str_detect(tolower(dta$recensione), "hopper|jim"), 1, 0)
dta$joyce <- ifelse(str_detect(tolower(dta$recensione), "joyce"), 1, 0)
dta$will <- ifelse(str_detect((dta$recensione), "Will"), 1, 0) # soluzione più semplice per separare will come verbo da will protagonista
# visto che will come verbo è spesso minuscolo e l'anlisi del pos lavora peggio 
dta$max <- ifelse(str_detect(dta$recensione, "Max"), 1, 0) #si tiene maiuscolo anche con Max
dta$lucas <- ifelse(str_detect(tolower(dta$recensione), "lucas"), 1, 0)
dta$demogorgon <- ifelse(str_detect(tolower(dta$recensione), "demogorgon|demodog"), 1, 0)
dta$mike <- ifelse(str_detect(tolower(dta$recensione), "mike|michael"), 1, 0)
dta$dustin <- ifelse(str_detect(tolower(dta$recensione), "dustin|henderson"), 1, 0)
dta$nancy <- ifelse(str_detect(tolower(dta$recensione), "nancy"), 1, 0)
dta$steve <- ifelse(str_detect(tolower(dta$recensione), "steve|harrington"), 1, 0)
dta$jonathan <- ifelse(str_detect(tolower(dta$recensione), "jonathan"), 1, 0)
dta$billy <- ifelse(str_detect(tolower(dta$recensione), "billy|hargrove"), 1, 0)
dta$robin <- ifelse(str_detect(tolower(dta$recensione), "robin|buckley"), 1, 0)
dta$erica <- ifelse(str_detect(tolower(dta$recensione), "erica"), 1, 0)
dta$vecna <- ifelse(str_detect(tolower(dta$recensione), "vecna|henry"), 1, 0)
dta$eddie <- ifelse(str_detect(tolower(dta$recensione), "eddie|munson"), 1, 0)
dta$murray <- ifelse(str_detect(tolower(dta$recensione), "murray"), 1, 0)
dta$holly <- ifelse(str_detect(tolower(dta$recensione), "holly"), 1, 0)
dta$kali <- ifelse(str_detect(tolower(dta$recensione), "kali|eight"), 1, 0)

##### 1.Analisi descrittive #####

character_cols <- names(dta)[8:27]

### 1.a Frequenze assolute e percentuali citazioni personaggi
#crea un tibble cha ha sulle colonne freq percentuale e assoluta per ciascun personaggio
character_stats <- dta %>%
  summarise(across(all_of(character_cols), list(
    count = ~sum(. == 1, na.rm = TRUE),
    perc = ~mean(. == 1, na.rm = TRUE) * 100
  )))

# Riorganizza in formato più leggibile ( da formto largo a lungo, colonne personaggi, f.ass e f.perc)
character_stats_long <- character_stats %>%
  pivot_longer(everything(), 
               names_to = c("character", ".value"), #specifica le nuove colonne da creare, 
               #.value crea colonne con nomi dopo character_
               names_pattern = "(.*)_(count|perc)") %>% #specifica di prendere count e perc
  arrange(desc(count))

# Visualizzazione

ggplot(character_stats_long, aes(x = reorder(character, -perc), y = perc)) + #reorder ordina per percentuale
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Percentuale di recensioni che menzionano ogni personaggio",
       x = "Personaggio", y = "Percentuale (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Vediamo che will è il personaggio più menzionato, 
# seguito da Vecna e mike

##### 1.b Analizziamo adesso le mezioni per episodio ######
# Menzioni per episodio (dataset largo, colonne episodio e personaggi, f.perc)
mentions_by_episode <- dta %>%
  group_by(episodio) %>%
  summarise(across(all_of(character_cols), ~mean(. == 1, na.rm = TRUE)*100))

# Converti in formato lungo per visualizzazione
mentions_long <- mentions_by_episode %>%
  pivot_longer(-episodio, names_to = "character", values_to = "mentions") # mantiene colonna episodio

# Heatmap delle menzioni
ggplot(mentions_long, aes(x = episodio, y = character, fill = mentions)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "Menzioni dei personaggi per episodio",
       x = "Episodio", y = "Personaggio") +
  theme_minimal()

# vediamo che will è mezionato soprattuttto negli episodi 4 e 7, dove ha grande importanza, 
# max soprattutto nel sesto (esce dalla "gabbia di vecna"), nell'episodio 2 
# Holly e i demogorgoni sono spesso citati, in quanto questi ultimi la rapiscono
# Vecna citato spesso nell'ultimo episodio, in cui ha ovviamente grande importanza.
# Nancy e jonathan nel sesto episodio (ha fine la loro relazione romantica)

##### 1.c Analizziamo le co-occorrenze dei personaggi nelle recensioni ######

# Matrice di co-occorrenza
cooccurrence_matrix <- matrix(0, nrow = length(character_cols), ncol = length(character_cols))
rownames(cooccurrence_matrix) <- character_cols
colnames(cooccurrence_matrix) <- character_cols

# Calcola co-occorrenze
for (i in 1:length(character_cols)) {
  for (j in 1:length(character_cols)) {
    if (i != j) {
      cooccurrence_matrix[i, j] <- sum(dta[[character_cols[i]]] == 1 & dta[[character_cols[j]]] == 1, na.rm = TRUE)
    }
  }
}

# Trasformiamo matrice in formato lungo
cooccurrence_melted <- melt(cooccurrence_matrix) # melt converte oggetto da formato largo a formato
# lungo

# da lavorarci su, idea carina ma fa schifo (heatmap co-occorenze)
ggplot(cooccurrence_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Matrice di co-occorrenza dei personaggi",
       x = "Personaggio", y = "Personaggio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### 1.d Analisi rating per personaggio ######

# Confronta rating medio per recensioni che menzionano/non menzionano un personaggio
rating_comparison <- data.frame()

# crea dataframe con colonne personaggi, media voto, n_review e mezionato si/no
for (char in character_cols) {
  temp <- dta %>%
    group_by(!!sym(char)) %>%
    summarise(
      mean_rating = mean(rating, na.rm = TRUE),
      n_reviews = n()
    ) %>%
    mutate(character = char, mentioned = ifelse(.[[1]] == 1, "Sì", "No"))
  
  rating_comparison <- bind_rows(rating_comparison, temp)
}

# Visualizza
ggplot(rating_comparison, aes(x = character, y = mean_rating, fill = mentioned)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Rating medio per recensioni con/senza menzione del personaggio",
       x = "Personaggio", y = "Rating medio", fill = "Menzione?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### differenza rating con cit senza cit in positivo per dustin, lucas, Steve, holly, eddie
# in negativo per mike, demogorgone, hopper



##### 2.Preprocessing ######

# Estrae le parole più utilizzate nei post
dta %>%
  unnest_tokens(word, recensione, to_lower = TRUE) %>%
  count(word, sort = TRUE) %>%
  top_n(20)

# Estrae le parole più utilizzate nei titoli dei post escludendo stopwords

dta %>%
  unnest_tokens(word, recensione, to_lower = TRUE) %>%
  count(word, sort = TRUE) %>%
  anti_join(get_stopwords(language = "en", source="smart")) %>%
  top_n(50)

stoplist <- get_stopwords(language = "en", source = "smart") 
stoplist <- stoplist %>% filter(word != "will")

# Preprocessing testi: tokenizzazione, rimozione stopwords, rimozione numeri e punteggiatura

# Unigrammi o parole senza stopwords, numeri e punteggiatura

#prendiamo singoli token puliti ed eliminaando stop word
tt_ww <- dta %>% 
  select(recensione,id, eleven,max, will, lucas, dustin, mike) %>% 
  unnest_tokens(word, recensione, to_lower=FALSE) %>%
  filter(word != "will") %>% 
  mutate(word=tolower(word)) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>% #togli numeri
  filter(!str_detect(word, "[[:punct:]]+")) %>%#togli punteggiatura 
  anti_join(stoplist, by = "word")
##### analisi associazioni ######

### funzione per effettuare findAssocs con correlazione di Spearman
findAssocs.DocumentTermMatrix_sp <- function(x, terms, corlimit) {
  if (length(terms) != 1L) 
    stop("'terms' must be a single term")
  
  i <- match(terms, colnames(x))
  if (is.na(i)) 
    stop("'terms' not found")
  
  # Transpose to treat as term-document matrix
  m <- as.matrix(t(x))
  cor_m <- cor(t(m), method="spearman")
  
  # Extract correlations for the specific term
  cors <- cor_m[, i]
  
  # Filter by correlation limit and remove self-correlation
  cors <- cors[cors >= corlimit]
  cors <- cors[names(cors) != terms]
  
  # Sort in descending order
  sort(cors, decreasing = TRUE)
}

dtm <- 

#abbastanza lunga da far girare
ass_mike <- findAssocs.DocumentTermMatrix_sp(dtm, "mike", 0.1)
ass_el <- findAssocs.DocumentTermMatrix_sp(dtm, "eleven", 0.1)
ass_max <- findAssocs.DocumentTermMatrix_sp(dtm, "max", 0.1)
ass_dustin <- findAssocs.DocumentTermMatrix_sp(dtm, "dustin", 0.1)
ass_will <- findAssocs.DocumentTermMatrix_sp(dtm, "will", 0.1)
ass_lucas <- findAssocs.DocumentTermMatrix_sp(dtm, "lucas", 0.1)

# visualizziamo le associazioni ottenute tutte assieme graficamente



##### analisi tf_idf #####

#controlliamo in dta le recensioni in cui compare uno solo dei personaggi principali

key_dta = dta %>%
  filter(eleven + max + will + lucas + dustin + mike == 1) %>%
  select(id, recensione, eleven, max, will, lucas, dustin, mike)

# crea nuova colonna personaggi che ha valore numerico dipendentemente dal personaggio citato ed elimina
# le vecchie colonne dei personaggi

key_dta = key_dta %>%
  mutate(personaggio = case_when(
    eleven == 1 ~ "eleven",
    max == 1 ~ "max",
    will == 1 ~ "will",
    lucas == 1 ~ "lucas",
    dustin == 1 ~ "dustin",
    mike == 1 ~ "mike"
  )) %>%
  select(id, recensione, personaggio)

# new_stopwords <- c("mike", "will", "byers", "shnapp", "dustin", "lucas", "eleven", "max", "william") 
#           
# new_stoplist <- rbind(stoplist, data.frame(word = new_stopwords, lexicon = rep("custom", length(new_stopwords))))
# 
# 
key_tt_ww <- key_dta %>%
  select(recensione,id, personaggio) %>%
  unnest_tokens(word, recensione, to_lower=FALSE) %>%
  mutate(word=tolower(word)) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>% #togli numeri
  filter(!str_detect(word, "[[:punct:]]+")) %>% #togli punteggiatura
  anti_join(stoplist, by = "word")



# # calcola tf-idf per personaggio non esce molto interessante
# 
# key_tt_ww = key_tt_ww %>%
#   group_by(personaggio) %>%
#   mutate(tot=n()) %>%
#   ungroup()
# 
# key_tf_idf <- key_tt_ww %>%
#   count(personaggio, word, sort = TRUE) %>%
#   bind_tf_idf(word, personaggio, n) %>%
#   arrange(desc(tf_idf)) 


#vediamo i termini più frequenti nelle recnsioni per ciascun personaggio principale
print(key_tf_idf %>% filter(personaggio=="will"), n=60)
print(key_tf_idf %>% filter(personaggio=="lucas"), n=60)
print(key_tf_idf %>% filter(personaggio=="dustin"), n=60)
print(key_tf_idf %>% filter(personaggio=="mike"), n=60)
print(key_tf_idf %>% filter(personaggio=="eleven"), n=60)
print(key_tf_idf %>% filter(personaggio=="max"), n=60)



##### 3 Keyword Extraction with RAKE #####

# per prima cosa, "annotatiamo" il nostro corpus di recensioni con udpipe
# dobbiamo prima di tutto scaricare un modello di linguaggio
model <- udpipe_download_model(language = "english")  

# carichiamo il modello
model <- udpipe_load_model(model$file_model)

#controlliamo in dta le recensioni in cui compare uno solo dei personaggi principali

dta_r = dta %>%
  select(recensione, id, eleven, max, will, lucas, dustin, mike)

# crea nuova colonna personaggi che ha valore numerico dipendentemente dal personaggio citato ed elimina
# le vecchie colonne dei personaggi

##### mike ######

mike_dta <- dta_r %>% filter(mike==1)

# annotiamo i testi
annotated_carachter_mike <- udpipe_annotate(model, x = mike_dta$recensione)
annotated_carachter_mike <- as.data.frame(annotated_carachter_mike)

# applichiamo algoritmo RAKE 
keywords_mike <- keywords_rake(
  x = annotated_carachter_mike ,
  term = "token",
  group = "doc_id", 
  relevant = annotated_carachter_mike$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

# ordiniamo per RAKE score
keywords_mike <- keywords_mike %>% arrange(desc(rake))
head(keywords_mike, 30)

##### eleven ######
eleven_dta <- dta_r %>% filter(eleven==1)
# annotiamo i testi
annotated_carachter_eleven <- udpipe_annotate(model, x = eleven_dta$recensione)
annotated_carachter_eleven <- as.data.frame(annotated_carachter_eleven)
#applichiamo algoritmo
keywords_eleven <- keywords_rake(
  x = annotated_carachter_eleven , 
  term="token", 
  group="doc_id",
  relevant = annotated_carachter_eleven$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

##### dustin ######
dustin_dta <- dta_r %>% filter(dustin==1)
# annotiamo i testi
annotated_carachter_dustin <- udpipe_annotate(model, x = dustin_dta$recensione)
annotated_carachter_dustin <- as.data.frame(annotated_carachter_dustin)
#applichiamo algoritmo
keywords_dustin <- keywords_rake(
  x = annotated_carachter_dustin , 
  term="token", 
  group="doc_id",
  relevant = annotated_carachter_dustin$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

##### lucas ######
lucas_dta <- dta_r%>% filter(lucas==1)
# annotiamo i testi
annotated_carachter_lucas <- udpipe_annotate(model, x = lucas_dta$recensione)
annotated_carachter_lucas <- as.data.frame(annotated_carachter_lucas)
# applichiamo algoritmo
keywords_lucas <- keywords_rake(
  x = annotated_carachter_lucas , 
  term="token", 
  group="doc_id",
  relevant = annotated_carachter_lucas$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

##### max ######
max_dta <- dta_r %>% filter(max==1)
# annotiamo i testi
annotated_carachter_max <- udpipe_annotate(model, x = max_dta$recensione)
annotated_carachter_max <- as.data.frame(annotated_carachter_max)
# applichiamo algoritmo
keywords_max <- keywords_rake(
  x = annotated_carachter_max , 
  term="token", 
  group="doc_id",
  relevant = annotated_carachter_max$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

##### will ######
will_dta <- dta_r %>% filter(will==1)
# annotiamo i testi
annotated_carachter_will <- udpipe_annotate(model, x = will_dta$recensione)
annotated_carachter_will <- as.data.frame(annotated_carachter_will)
#applichiamo algoritmo
keywords_will <- keywords_rake(
  x = annotated_carachter_will , 
  term="token", 
  group="doc_id",
  relevant = annotated_carachter_will$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)


# View top keywords
head(keywords_will, 20)
head(keywords_mike, 20)
head(keywords_dustin, 20)
head(keywords_lucas, 20)
head(keywords_eleven, 20)
head(keywords_max, 20)
 

##### Rake per frase #####
# proviamo a fare rake utilizzando come unità di testo la frase e non la recensione, dato che
# sembra esserci parecchio rumore utilizzando la recensione

sen_dta <-dta %>%
  select(id, recensione) %>%
  unnest_tokens(
    output = frase, 
    input = recensione, 
    token = "ngrams", 
    n= 6, 
    to_lower = FALSE,    # mantieni maiuscole per nomi propri
    drop = FALSE        # mantieni colonna originale
  ) %>%
  mutate(
    frase_id = paste0(id, "_", row_number()) #id unico per frase
  )

sen_dta$eleven <- ifelse(str_detect(tolower(sen_dta$frase), "eleven|Jane|\b el \b"), 1, 0)
sen_dta$will <- ifelse(str_detect((sen_dta$frase), "Will"), 1, 0) # soluzione più semplice per separare will come verbo da will protagonista
# visto che will come verbo è spesso minuscolo e l'anlisi del pos lavora peggio 
sen_dta$max <- ifelse(str_detect(sen_dta$frase, "Max"), 1, 0) #si tiene maiuscolo anche con Max
sen_dta$lucas <- ifelse(str_detect(tolower(sen_dta$frase), "lucas"), 1, 0)
sen_dta$mike <- ifelse(str_detect(tolower(sen_dta$frase), "mike|michael"), 1, 0)
sen_dta$dustin <- ifelse(str_detect(tolower(sen_dta$frase), "dustin|henderson"), 1, 0)

#controlliamo in dta le recensioni in cui compare uno solo dei personaggi principali

sen_dta = sen_dta %>%
  select(frase_id, frase, eleven, max, will, lucas, dustin, mike)

# crea nuova colonna personaggi che ha valore numerico dipendentemente dal personaggio citato ed elimina
# le vecchie colonne dei personaggi

##### mike ######

mike_dta_s <- sen_dta %>% filter(mike==1)

# annotiamo i testi
annotated_carachter_mike_s <- udpipe_annotate(model, x = mike_dta_s$frase)
annotated_carachter_mike_s <- as.data.frame(annotated_carachter_mike_s)

# applichiamo algoritmo RAKE 
keywords_mike_s <- keywords_rake(
  x = annotated_carachter_mike_s ,
  term = "token",
  group = "doc_id", 
  relevant = annotated_carachter_mike_s$upos %in% c("NOUN", "ADJ"),
  ngram_max = 1
)

# ordiniamo per RAKE score
keywords_mike_s <- keywords_mike_s %>% arrange(desc(rake))
head(keywords_mike_s, 30)

##### eleven ######

eleven_dta_s <- sen_dta %>% filter(eleven==1)

# annotiamo i testi
annotated_carachter_eleven_s <- udpipe_annotate(model, x = eleven_dta_s$frase)
annotated_carachter_eleven_s <- as.data.frame(annotated_carachter_eleven_s)

# applichiamo algoritmo RAKE 
keywords_eleven_s <- keywords_rake(
  x = annotated_carachter_eleven_s ,
  term = "token",
  group = "doc_id", 
  relevant = annotated_carachter_eleven_s$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

# ordiniamo per RAKE score
keywords_eleven_s <- keywords_eleven_s %>% arrange(desc(rake))
head(keywords_eleven_s, 30)

##### dustin ######

dustin_dta_s <- sen_dta %>% filter(dustin==1)

# annotiamo i testi
annotated_carachter_dustin_s <- udpipe_annotate(model, x = dustin_dta_s$frase)
annotated_carachter_dustin_s <- as.data.frame(annotated_carachter_dustin_s)

# applichiamo algoritmo RAKE 
keywords_dustin_s <- keywords_rake(
  x = annotated_carachter_dustin_s ,
  term = "token",
  group = "doc_id", 
  relevant = annotated_carachter_dustin_s$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

# ordiniamo per RAKE score
keywords_dustin_s <- keywords_dustin_s %>% arrange(desc(rake))
head(keywords_dustin_s, 30)

##### lucas ######

lucas_dta_s <- sen_dta %>% filter(lucas==1)

# annotiamo i testi
annotated_carachter_lucas_s <- udpipe_annotate(model, x = lucas_dta_s$frase)
annotated_carachter_lucas_s <- as.data.frame(annotated_carachter_lucas_s)

# applichiamo algoritmo RAKE 
keywords_lucas_s <- keywords_rake(
  x = annotated_carachter_lucas_s ,
  term = "token",
  group = "doc_id", 
  relevant = annotated_carachter_lucas_s$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

# ordiniamo per RAKE score
keywords_lucas_s <- keywords_lucas_s %>% arrange(desc(rake))
head(keywords_lucas_s, 30)

##### max ######

max_dta_s <- sen_dta %>% filter(max==1)

# annotiamo i testi
annotated_carachter_max_s <- udpipe_annotate(model, x = max_dta_s$frase)
annotated_carachter_max_s <- as.data.frame(annotated_carachter_max_s)

# applichiamo algoritmo RAKE 
keywords_max_s <- keywords_rake(
  x = annotated_carachter_max_s ,
  term = "token",
  group = "doc_id", 
  relevant = annotated_carachter_max_s$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

# ordiniamo per RAKE score
keywords_max_s <- keywords_max_s %>% arrange(desc(rake))
head(keywords_max_s, 30)

##### will ######

will_dta_s <- sen_dta %>% filter(will==1)

# annotiamo i testi
annotated_carachter_will_s <- udpipe_annotate(model, x = will_dta_s$frase)
annotated_carachter_will_s <- as.data.frame(annotated_carachter_will_s)

# applichiamo algoritmo RAKE 
keywords_will_s <- keywords_rake(
  x = annotated_carachter_will_s ,
  term = "token",
  group = "doc_id", 
  relevant = annotated_carachter_will_s$upos %in% c("NOUN", "ADJ"), 
  ngram_max = 1
)

# ordiniamo per RAKE score
keywords_will_s <- keywords_will_s %>% arrange(desc(rake))
head(keywords_will_s, 30)


##### 4. Topic Modeling ##### 

#aggiungere stop words, fa schifo

library(topicmodels)
library(ldatuning)
library(quanteda)

# 1. Preprocess text

dfm <- dta$recensione %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem() %>%  # or lemmatize
  tokens_ngrams(n = 1:2) %>%  # optional: include bigrams
  dfm()

# 2. Trim sparse terms
dfm <- dfm_trim(dfm, min_docfreq = 2, docfreq_type = "count")

# 3. Convert to topicmodels format
dtm <- convert(dfm, to = "topicmodels")

# 4. Find optimal topics
result <- FindTopicsNumber(
  dtm,
  topics = seq(4, 20, by = 2),
  metrics = c("CaoJuan2009", "Griffiths2004"),
  mc.cores = 1L,
  verbose = TRUE
)

# 5. Fit LDA with optimal topics
optimal_k <- result$topics[which.min(result$CaoJuan2009)]
lda_model <- LDA(dtm, k = optimal_k, method = "Gibbs", control = list(seed = 1234))

# Mostra le probabilità delle parole nei topic
char_topics <- tidy(lda_model, matrix = "beta")

# Mostra le parole più frequenti per ciascun topic
char_top_terms <- char_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Crea un grafico a barre delle parole più frequenti per ciascun topic
char_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
# 
# 
# key_tt_ww = key_tt_ww %>% group_by(word) %>% mutate(tot=n())
# 
# key_dtm = key_tt_ww %>% cast_dtm(document = id,term = word,value = tot) 
# 
# char_lda = LDA(key_dtm, k = 6, method="Gibbs", control = list(seed = 1123, burnin=1000, iter=1000))
# 
# # Mostra le probabilità delle parole nei topic
# char_topics <- tidy(char_lda, matrix = "beta")
# char_topics
# 
# # Mostra le parole più frequenti per ciascun topic
# char_top_terms <- char_topics %>%
#   group_by(topic) %>%
#   slice_max(beta, n = 10) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# # Crea un grafico a barre delle parole più frequenti per ciascun topic
# char_top_terms %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   ggplot(aes(beta, term, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   scale_y_reordered()
# 
# # non viene soddisfacente, facciamo topic modeling su dataset per personaggio
# 
# lista_char <- split(key_tt_ww, key_tt_ww$personaggio)
# 
# ### mike
# 
# mike_dtm <- lista_char$mike %>% cast_dtm(document = id,term = word,value = tot) 
# 
# FindTopicsNumber(mike_dtm, topics= seq(4, 20, by=2), metrics = "CaoJuan2009")




##### 5.Sentiment Analysis #####

tokens <- dta %>%
  unnest_tokens(word, recensione, to_lower = TRUE)

# Supponiamo di avere una lista di parole di negazione
negation_words <- c("not", "no", "never")

# Dobbiamo fare in modo che ogni parola abbia un indicatore se è negata
# usiamo un approccio a finestra: se una parola è preceduta da una negazione in una finestra di n parole, allora è negata.

# Funzione per segnare le parole negate
mark_negations <- function(tokens, negation_words, window = 3) {
  tokens$negated <- FALSE
  for (i in 1:nrow(tokens)) {
    # Controlla le parole precedenti in una finestra
    start <- max(1, i - window)
    for (j in start:i) {
      if (tokens$word[j] %in% negation_words) {
        tokens$negated[i] <- TRUE
        break
      }
    }
  }
  tokens
}

tokens <- mark_negations(tokens, negation_words)

# Ora unisci con i lessemi di sentimento (AFINN)
afinn <- get_sentiments("afinn")

tokens_with_sentiment <- tokens %>%
  left_join(afinn, by = "word")

# Per le parole negate, invertiamo il punteggio (moltiplicando per -1)
tokens_with_sentiment <- tokens_with_sentiment %>%
  mutate(value = ifelse(negated, -value, value))

# Calcola il sentimento per documento
sentiment <- tokens_with_sentiment %>%
  group_by(id) %>%
  summarise(sentiment = sum(value, na.rm = TRUE))

# Aggiungi i sentiment scores al dataframe originale con left_join
dta <- dta %>%
  left_join(sentiment, by = "id") %>%
  # Sostituisci NA con 0 per i documenti senza parole nel lessico
  mutate(sentiment = coalesce(sentiment, 0))

# vediamo sentiment medio per recensionin in cui compare il personaggio
sentiment_by_character <- data.frame()

for (char in character_cols) {
  temp <- dta %>%
    group_by(!!sym(char)) %>%
    summarise(
      mean_sentiment = mean(sentiment, na.rm = TRUE),
      n_reviews = n()
    ) %>%
    mutate(character = char, mentioned = ifelse(.[[1]] == 1, "Sì", "No"))
  
  sentiment_by_character <- bind_rows(sentiment_by_character, temp)
}

# diamo una rappresentazione grafica

ggplot(sentiment_by_character, aes(x = character, y = mean_sentiment, fill = mentioned)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment medio per recensioni con/senza menzione del personaggio",
       x = "Personaggio", y = "Sentiment medio", fill = "Menzione?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#facciamo la stessa analisi con afinn normalizzato in -1, 0 e 1

tokens_with_sentiment_n <- tokens_with_sentiment %>%
  mutate(value = case_when(
    value > 0 ~ 1,
    value < 0 ~ -1,
    value=NA ~ 0
  ))

# Calcola il sentimento per documento
sentiment_n <- tokens_with_sentiment_n %>%
  group_by(id) %>%
  summarise(sen = sum(value, na.rm = TRUE))

# Aggiungi i sentiment scores al dataframe originale con left_join
dta <- dta %>%
  left_join(sentiment_n, by = "id") %>%
  # Sostituisci NA con 0 per i documenti senza parole nel lessico
  mutate(sentiment_n = coalesce(sen, 0))

# vediamo sentiment medio per recensionin in cui compare il personaggio
sentiment_by_character_n <- data.frame()

for (char in character_cols) {
  temp <- dta %>%
    group_by(!!sym(char)) %>%
    summarise(
      mean_sentiment_n = mean(sentiment_n, na.rm = TRUE),
      n_reviews = n()
    ) %>%
    mutate(character = char, mentioned = ifelse(.[[1]] == 1, "Sì", "No"))
  
  sentiment_by_character_n <- bind_rows(sentiment_by_character_n, temp)
}

# diamo una rappresentazione grafica

ggplot(sentiment_by_character_n, aes(x = character, y = mean_sentiment_n, fill = mentioned)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment medio per recensioni con/senza menzione del personaggio",
       x = "Personaggio", y = "Sentiment medio", fill = "Menzione?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##### Analisi sentiment bing

bing <- get_sentiments("bing")

tokens_bing <- tokens %>%
  left_join(bing, by = "word") %>%
  mutate(sentiment=ifelse(sentiment=="negative", -1, 1))
  

# Per le parole negate, invertiamo il punteggio (moltiplicando per -1)
tokens_bing <- tokens_bing %>%
  mutate(value = ifelse(negated, -sentiment, sentiment))

# Calcola il sentimento per documento
sentiment_bing <- tokens_bing %>%
  group_by(id) %>%
  summarise(sent = sum(value, na.rm = TRUE))

# Aggiungi i sentiment scores al dataframe originale con left_join
dta <- dta %>%
  left_join(sentiment_bing, by = "id") %>%
  # Sostituisci NA con 0 per i documenti senza parole nel lessico
  mutate(sentiment_b = coalesce(sent, 0))

# vediamo sentiment medio per recensionin in cui compare il personaggio
sentiment_by_character_b <- data.frame()

for (char in character_cols) {
  temp <- dta %>%
    group_by(!!sym(char)) %>%
    summarise(
      mean_sentiment_b = mean(sentiment_b, na.rm = TRUE),
      n_reviews = n()
    ) %>%
    mutate(character = char, mentioned = ifelse(.[[1]] == 1, "Sì", "No"))
  
  sentiment_by_character_b <- bind_rows(sentiment_by_character_b, temp)
}

# diamo una rappresentazione grafica

ggplot(sentiment_by_character_b, aes(x = character, y = mean_sentiment_b, fill = mentioned)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment medio per recensioni con/senza menzione del personaggio",
       x = "Personaggio", y = "Sentiment medio", fill = "Menzione?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#confronto statistico tra sentiment con bing e con afinn normalizzato

cor(dta$sentiment_n, dta$sentiment_b, method="spearman", use="complete.obs")
# alta correlazione positiva tra i due metodi di sentiment analysis

#effetuiamo dei test tra i due sentiment

for (char in character_cols) {
  cat("Character:", char, "\n")
  
  # Sentiment AFINN normalizzato
  afinn_data <- dta %>%
    filter(!!sym(char) == 1) %>%
    pull(sentiment_n)
  
  # Sentiment Bing
  bing_data <- dta %>%
    filter(!!sym(char) == 1) %>%
    pull(sentiment_b)
  
  # Test di Wilcoxon
  test_result <- wilcox.test(afinn_data, bing_data, paired = TRUE)
  
  cat("Wilcoxon test p-value:", test_result$p.value, "\n\n")
}
# per tutti i personaggi il p-value è <<0.05, quindi rifiutiamo l'ipotesi nulla, risultati ottenuti scostano molto
# si notava già graficamente dato che con bing otteniamo sentiment negativi, con afinn sentiment positivi

#test su dataset completo

afinn_data_all <- dta %>%
  pull(sentiment_n)

bing_data_all <- dta %>%
  pull(sentiment_b)

# Test di Wilcoxon
test_result_all <- wilcox.test(afinn_data_all, bing_data_all, paired = TRUE)
cat("Wilcoxon test p-value (all data):", test_result_all$p.value, "\n")
# anche sul dataset completo otteniamo p-value <<.05, quindi i due metodi danno risultati completamente diversi