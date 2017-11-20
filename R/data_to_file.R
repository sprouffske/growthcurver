# Save data in a seperate file

data_to_file <- function(gc_out){
    fileOut<-file("growth_Summ_Output.txt")
    write.table(gc_out, fileOut, quote = FALSE, sep = "\t", row.names = FALSE)
}
