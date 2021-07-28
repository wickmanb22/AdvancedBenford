delta_notelength <- function(midi_file){
  notes <- readMidi(midi_file)
  values <- getMidiNotes(notes)
  cumvalues <- values %>% group_by(notename) %>% summarize(cum_length = sum(length))
  sig1 <- extract.digits(cumvalues$cum_length, number.of.digits = 1)
  cumsig1 <- sig1 %>%group_by(data.digits) %>% summarize(n = n())
  cumsig1$pct <- cumsig1$n / sum(cumsig1$n)
  cleandata <- cumsig1 %>%mutate(n=NULL)
  cleandata$benf <- log10(1+1/cleandata$data.digits)
  cleandata$diff <- abs(cleandata$pct - cleandata$benf)
  cleandata1 <- cleandata %>% arrange(desc(diff))
  digit <- data.frame(cleandata1[1,1],100*cleandata1[1,4])
  colnames(digit)[1] <- "Digit of Maximum Difference"
  colnames(digit)[2] <- "Delta"
  return(digit)
}

delta_freq <- function(midi_file){
  notes <- readMidi(midi_file)
  values <- getMidiNotes(notes)
  notes1 <- values %>% mutate(freq=440*2^((note-69)/12))
  cumvalues <- notes1 %>% group_by(note) %>% summarize(cum_freq = sum(freq))
  sig1 <- extract.digits(cumvalues$cum_freq, number.of.digits = 1)
  cumsig1 <- sig1 %>%group_by(data.digits) %>% summarize(n = n())
  cumsig1$pct <- cumsig1$n / sum(cumsig1$n)
  cleandata <- cumsig1 %>%mutate(n=NULL)
  cleandata$benf <- log10(1+1/cleandata$data.digits)
  cleandata$diff <- abs(cleandata$pct - cleandata$benf)
  cleandata1 <- cleandata %>% arrange(desc(diff))
  digit <- data.frame(cleandata1[1,1],100*cleandata1[1,4])
  colnames(digit)[1] <- "Digit of Maximum Difference"
  colnames(digit)[2] <- "Delta"
  return(digit)}

benford_categorization <- function(midi_file){
  freq_deltas <- delta_freq(midi_file)
  freq_deltas1 <- data.frame(c(Type = "Wavelength",freq_deltas))
  notelength_deltas <- delta_notelength(midi_file)
  notelength_deltas1 <- data.frame(c(Type="Note Length",notelength_deltas))
  clean_df <- rbind(freq_deltas1,notelength_deltas1)
  categorization_df <- mutate(clean_df, Categorization = ifelse(Digit.of.Maximum.Difference == 1 & Delta > 8.92, "Not Benford",
                                                                ifelse(Digit.of.Maximum.Difference == 1 & Delta < 7.24, "Benford",
                                                                       ifelse(Digit.of.Maximum.Difference == 1 & Delta < 7.96 & Delta >= 7.24, "Moderately Benford",
                                                                              ifelse(Digit.of.Maximum.Difference == 1 & Delta <= 8.92 & Delta >= 7.96, "Slightly Benford",
                                                                                     ifelse(Digit.of.Maximum.Difference == 2 & Delta < 6.04, "Benford",
                                                                                            ifelse(Digit.of.Maximum.Difference == 2 & Delta > 7.4, "Not Benford",
                                                                                                   ifelse(Digit.of.Maximum.Difference == 2 & Delta < 6.52 & Delta >= 6.04 , "Moderately Benford",
                                                                                                          ifelse(Digit.of.Maximum.Difference == 2 & Delta <= 7.4 & Delta >= 6.52 , "Slightly Benford",
                                                                                                                 ifelse(Digit.of.Maximum.Difference == 3 & Delta < 5.24, "Benford",
                                                                                                                        ifelse(Digit.of.Maximum.Difference == 3 & Delta > 6.44, "Not Benford",
                                                                                                                               ifelse(Digit.of.Maximum.Difference == 3 & Delta < 5.64 & Delta >= 5.24, "Moderately Benford",
                                                                                                                                      ifelse(Digit.of.Maximum.Difference == 3 & Delta <= 6.44 & Delta >= 5.64, "Slightly Benford",
                                                                                                                                             ifelse(Digit.of.Maximum.Difference == 4 & Delta < 4.76, "Benford",
                                                                                                                                                    ifelse(Digit.of.Maximum.Difference == 4 & Delta > 5.8, "Not Benford",
                                                                                                                                                           ifelse(Digit.of.Maximum.Difference == 4 & Delta < 5.08 & Delta >= 4.76, "Moderately Benford",
                                                                                                                                                                  ifelse(Digit.of.Maximum.Difference == 4 & Delta <= 5.8 & Delta >= 5.08, "Slightly Benford",
                                                                                                                                                                         ifelse(Digit.of.Maximum.Difference == 5 & Delta < 4.36, "Benford",
                                                                                                                                                                                ifelse(Digit.of.Maximum.Difference == 5 & Delta > 5.32, "Not Benford",
                                                                                                                                                                                       ifelse(Digit.of.Maximum.Difference == 5 & Delta < 4.68 & Delta >= 4.36, "Moderately Benford",
                                                                                                                                                                                              ifelse(Digit.of.Maximum.Difference == 5 & Delta <= 5.32 & Delta >= 4.68, "Slightly Benford",
                                                                                                                                                                                                     ifelse(Digit.of.Maximum.Difference == 6 & Delta < 4.04, "Benford",
                                                                                                                                                                                                            ifelse(Digit.of.Maximum.Difference == 6 & Delta > 4.92, "Not Benford",
                                                                                                                                                                                                                   ifelse(Digit.of.Maximum.Difference == 6 & Delta < 4.38 & Delta >= 4.04, "Moderately Benford",
                                                                                                                                                                                                                          ifelse(Digit.of.Maximum.Difference == 6 & Delta <= 4.92 & Delta >= 4.38, "Slightly Benford",
                                                                                                                                                                                                                                 ifelse(Digit.of.Maximum.Difference == 7 & Delta < 3.8, "Benford",
                                                                                                                                                                                                                                        ifelse(Digit.of.Maximum.Difference == 7 & Delta > 4.6, "Not Benford",
                                                                                                                                                                                                                                               ifelse(Digit.of.Maximum.Difference == 7 & Delta < 4.12 & Delta >= 3.8, "Moderately Benford",
                                                                                                                                                                                                                                                      ifelse(Digit.of.Maximum.Difference == 7 & Delta <= 4.6 & Delta >= 4.12, "Slightly Benford",
                                                                                                                                                                                                                                                             ifelse(Digit.of.Maximum.Difference == 8 & Delta < 3.56, "Benford",
                                                                                                                                                                                                                                                                    ifelse(Digit.of.Maximum.Difference == 8 & Delta > 4.36, "Not Benford",
                                                                                                                                                                                                                                                                           ifelse(Digit.of.Maximum.Difference == 8 & Delta < 3.88 & Delta >= 3.56, "Moderately Benford",
                                                                                                                                                                                                                                                                                  ifelse(Digit.of.Maximum.Difference == 8 & Delta <= 4.36 & Delta >= 3.88, "Slightly Benford",
                                                                                                                                                                                                                                                                                         ifelse(Digit.of.Maximum.Difference == 9 & Delta < 3.4, "Benford",
                                                                                                                                                                                                                                                                                                ifelse(Digit.of.Maximum.Difference == 9 & Delta > 4.12, "Not Benford",
                                                                                                                                                                                                                                                                                                       ifelse(Digit.of.Maximum.Difference == 9 & Delta < 3.64 & Delta >= 3.4, "Moderately Benford",
                                                                                                                                                                                                                                                                                                              ifelse(Digit.of.Maximum.Difference == 9 & Delta <= 4.12 & Delta >= 3.64, "Slightly Benford",
                                                                                                                                                                                                                                                                                                                     0
                                                                                                                                                                                                                                                                                                              )))))))))))))))))))))))))))))))))))))

  categorization_df1 <- categorization_df[,-c(2,3)]
  return(categorization_df1)
}
