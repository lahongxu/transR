#' Translate DNA into protein
#' @param dna a vector of numeric values
#' @return protein sequence
#' @export
#' @examples
#' dna <- "ATCGCTGGA"
#' translate(dna)


translate <- function(dna){
  table <- read.table(file = system.file("codon.txt", package = "transR"), header = F, as.is = T)[ ,c(1,3)]

  dna <- toupper(dna)
  dna <- gsub("U","T",dna)
  dna
  num.codons <- nchar(dna)/3
  starts <- seq(from=1, by=3, length.out = num.codons)
  results <- c()
  codons <- list()
  for(i in 1:num.codons){
    codons[i] <- substring(dna,starts[i],starts[i]+2)
  }

  for(i in 1:length(codons)){
    results[i] <- table[table[,1]==codons[[i]],2]
  }
  return(paste(unlist(results),sep = "", collapse = ""))
}
