if(process_sequences == T){
  
  library(sangeranalyseR)
  library(seqinr)
  library(msa)
  library(tidyverse)
  library(kableExtra)
  library(pegas)
  library(adegenet)
  
  alignments = SangerAlignment(ABIF_Directory      = paste0("Raw_data/sequence_data/"),
                               processMethod       = "REGEX",
                               REGEX_SuffixForward = paste0("_F_.*ab1$"),
                               REGEX_SuffixReverse = paste0("_R_.*ab1$"))
  
  writeFasta(alignments, outputDir = "Output/Sequences/")
  
  generateReport(alignments,
                 outputDir = paste0("Output/Sequences/"),
                 includeSangerRead = T, 
                 includeSangerContig = F)
}


