# Analisi di Sentiment e Caratterizzazione dei Personaggi in *Stranger Things 5*

**Progetto di text mining su 7.937 recensioni IMDb: cosa pensa il pubblico dei personaggi dell'ultima stagione, e perché.**

Progetto universitario individuale: dalla raccolta dei dati (web scraping) all'analisi statistica e testuale, fino all'interpretazione dei risultati.

---

## In breve

| | |
|---|---|
| **Domanda** | Come viene percepito ogni personaggio principale dal pubblico, e come cambia il tono delle recensioni quando viene nominato? |
| **Dati** | 7.937 recensioni in inglese della quinta stagione, raccolte da IMDb il 19/01/2026 con Selenium |
| **Metodi** | Web scraping, analisi descrittiva, matrici di co-occorrenza (Jaccard), correlazione di Spearman su document-term matrix, estrazione di keyword con RAKE, sentiment analysis con due lessici (AFINN e Bing) |
| **Strumenti** | R (`tm`, `tidytext`, `udpipe`, `dplyr`, `ggplot2`), Python (Selenium, pandas) in Jupyter per lo scraping |
| **Output** | Report completo con grafici e interpretazione: [`docs/Report.pdf`](docs/Report.pdf) |

---

## Approccio

1. **Raccolta dati**: scraping delle recensioni con Selenium. Per ognuna ho estratto testo, titolo, voto (1-10), upvote, downvote ed episodio di riferimento.
2. **Pulizia e preparazione**: conversione delle variabili numeriche, gestione dei valori mancanti, creazione di 20 variabili dicotomiche (una per personaggio) che indicano se il personaggio è citato nella recensione.
3. **Analisi esplorativa**: frequenza di citazione dei personaggi, andamento delle menzioni episodio per episodio, co-occorrenze tra personaggi normalizzate con l'indice di Jaccard.
4. **Impatto sul voto**: per ogni personaggio, differenza tra il voto medio delle recensioni che lo citano e di quelle che non lo citano.
5. **Caratterizzazione lessicale**: correlazione di Spearman tra ogni termine e la menzione di un personaggio (adattando `findAssocs` di `tm`), poi estrazione di parole chiave con RAKE su n-grammi di 6 parole, filtrando solo sostantivi e aggettivi.
6. **Sentiment analysis**: confronto tra due lessici (AFINN, con punteggi pesati, e Bing, binario) con gestione delle negazioni. Validazione incrociata con correlazione di Spearman e test di Wilcoxon appaiato.

---

## Risultati principali

- **I personaggi più discussi** sono Will (citato in circa il 29% delle recensioni), Vecna (circa 27%), Mike e Eleven.
- **I picchi di menzioni seguono la trama**: ad esempio Will nel 4° e 7° episodio (rivelazione dei poteri e coming out) e Nancy e Jonathan nel 6°. Le menzioni per episodio funzionano come un "termometro" degli eventi narrativi.
- **Impatto sul voto**: le recensioni che citano Dustin, Lucas, Steve, Eddie e Holly hanno voti medi più alti. Quelle che citano Demogorgon, Mike, Hopper e Vecna hanno voti medi più bassi.
- **Impatto sul sentiment (AFINN)**: Lucas (+4.63), Robin (+4.27) e Dustin (+3.95) sono associati ai toni più positivi. Billy (−1.90), Demogorgon (−1.32) e Vecna (−1.11) ai più negativi. Per gli antagonisti è un risultato atteso: le scene cupe abbassano il tono delle recensioni.
- **Validazione**: i due lessici concordano sull'ordine relativo di positività (Spearman r ≈ 0.73), ma Bing produce punteggi sistematicamente più negativi (Wilcoxon, p < 0.05 per tutti i personaggi). Per questo AFINN è stato considerato la misura più sfumata.
- **RAKE sugli n-grammi** ha ridotto il rumore rispetto all'uso di intere recensioni come contesto, restituendo keyword più specifiche per personaggio (es. *elevator* e *close* per Lucas, *choices* per Will).

Il modello LDA per il topic modeling è stato provato ma scartato: non produceva topic interpretabili sulle recensioni, che sono brevi e molto focalizzate. Le altre limitazioni (lessici che non colgono ironia e sarcasmo, singola piattaforma, nessuna distinzione tra attore e scrittura) e gli sviluppi futuri (modelli basati su transformer come BERT, analisi nel tempo, altre piattaforme) sono discussi nel report.

---

## Struttura della repository

```
.
├── Data/
│   ├── stranger_things.xlsx                  # Dataset: tutte le recensioni raccolte con i loro attributi
│   └── english-ewt-ud-2.5-191206.udpipe      # Modello linguistico udpipe (POS tagging per RAKE)
├── Scripts/
│   ├── Scraping-Imdb.ipynb                   # Notebook di raccolta dati da IMDb (Selenium)
│   └── Stranger.R                            # Analisi completa in R
├── docs/
│   ├── Report.pdf                            # Report finale con grafici e interpretazione
│   └── Report.Rmd                              # Sorgente R Markdown da cui è generato il report
└── README.md
```

---

## Come riprodurre l'analisi

**Requisiti**: R (≥ 4.x) e i seguenti pacchetti:

```r
install.packages(c(
  "stringr", "readxl", "dplyr", "tidyr", "ggplot2", "reshape2",
  "tm", "tidytext", "textdata", "udpipe", "textclean",
  "topicmodels", "ldatuning", "quanteda"
))
```

1. Clona la repository:
   ```bash
   git clone https://github.com/MattiaBazzani/Sentiment-Analysis-Stranger-Things.git
   ```
2. **(Opzionale) Raccolta dati**: il notebook `Scripts/Scraping-Imdb.ipynb` esegue lo scraping con Selenium. Servono Python e Google Chrome, oltre ai pacchetti:
   ```bash
   pip install selenium pandas openpyxl
   ```
   Con Selenium ≥ 4.6 il driver di Chrome viene scaricato automaticamente, quindi non serve alcuna configurazione manuale. Lo scraping dipende dalla struttura della pagina IMDb, quindi potrebbe richiedere aggiustamenti se il sito cambia, e le recensioni degli ultimi episodi richiedono diversi minuti di attesa. Il risultato è già salvato in `Data/stranger_things.xlsx`, quindi questo passaggio si può saltare.
3. **Analisi**: apri `Scripts/Stranger.R` in RStudio e imposta la working directory sulla cartella `Scripts/` (menu *Session → Set Working Directory → To Source File Location*). Lo script legge dataset e modello linguistico da `../Data/`, quindi non serve modificare alcun percorso. Eseguilo poi dall'alto verso il basso.
4. Lo script è organizzato in sezioni: analisi descrittive, preprocessing, associazioni lessicali, estrazione di keyword con RAKE, topic modeling (LDA, poi scartato) e sentiment analysis.
5. Il passaggio RAKE usa il modello linguistico udpipe incluso in `Data/` (`english-ewt-ud-2.5-191206.udpipe`), quindi non è necessaria una connessione per scaricarlo.
6. Per rigenerare il report, apri `docs/code.Rmd` in RStudio ed esegui **Knit**.

---

## Autore

**Mattia Bazzani**
[LinkedIn](www.linkedin.com/in/mattia-bazzani-8a4051226) · [Email](mattia.bazzani.one@gmail.com)
