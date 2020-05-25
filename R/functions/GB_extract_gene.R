### extract number of sequences containing each gene for a given taxa

GB_extract_gene <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  gene_table <- setNames(data.frame(matrix(ncol = 27, nrow = 1)), c("taxa", paste0(gene_list), "n_NA", "tot_n_seq"))
  
  # loop through gene names and fill gene_table
  gene = ""
  for (gene in gene_list) {
    gene_table$taxa <- taxa_name
    
    # count number of sequences containing the given gene
    gene_table[gene] <- sum(grepl(gene, taxa_data_med$gene)) # grepl is like str_detect but it doesn't return NA when used on NA
    
    # number of sequences without a gene assignation
    gene_table$n_NA <- sum(is.na(taxa_data_med$gene))
    
    # total number of sequences
    gene_table$tot_n_seq <- length(taxa_data_med$access_num)
  }
  
  return(gene_table)
}