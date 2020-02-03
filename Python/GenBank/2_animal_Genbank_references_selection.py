
###############################################################
### To measure the execution time of the program
### Script taken from : https://stackoverflow.com/a/12344609/10890752
import atexit
from time import time, strftime, localtime
from datetime import timedelta

def secondsToStr(elapsed=None):
    if elapsed is None:
        return strftime("%Y-%m-%d %H:%M:%S", localtime())
    else:
        return str(timedelta(seconds=elapsed))

def log(s, elapsed=None):
    line = "="*40
    print(line)
    print(secondsToStr(), '-', s)
    if elapsed:
        print("Elapsed time:", elapsed)
    print(line)
    print()

def endlog():
    end = time()
    elapsed = end-start
    log("End Program", secondsToStr(elapsed))

start = time()
atexit.register(endlog)
log("Start Program")
###############################################################

###### loading animal nuclear gene names
gene_index = open("name_list_animal_nuclear_genes.txt", "r")
gene_names = []
gene_line_list = []
for line in gene_index:
	gene_line_list = line.split(",",1)
	gene_names.append(" " + gene_line_list[0].lower().replace('\n','') + " ") # makes a list of the possible gene names with a space before and after each name
gene_length = len(gene_names)
gene_search = ""


def select_genbank_references(filename, outf):
	fin = open(filename, "r")
	access_num=''
	parsing_definition = False
	definition = ""
	parsing_organism = False
	organism = []
	data = ""
	datalist = []
	to_be_written = False
	
	### this loop reads a GenBank .seq file line by line
	### every time a given "keyword" is met in the first 10 characters of a line
	### the associated information is stored in an object on which conditions are tested
	### when the keyword "LOCUS     " is met again, if the conditions were met, the GenBank reference is written in the output file.
	for line in fin:
		keyword = line[:10]
		if keyword == "LOCUS     ":
			access_num = line[12:20]
			if to_be_written:
				data = ''.join(datalist)
				outf.write('\n'+data)
			to_be_written = False
			data = ""
			datalist = []
			organism = []
			definition = ""
		datalist.append(line)
		if keyword == "DEFINITION":
			definition = line[12:].strip()
			parsing_definition = True
			continue
		if keyword == "ACCESSION ":
			parsing_definition = False
			continue
		if parsing_definition:
			definition = definition + ' ' + line.strip()
			continue
		if keyword == "  ORGANISM":
			parsing_organism = True
			organism = [line[11:].strip()]
			continue
		if keyword == "REFERENCE ":
			parsing_organism = False
			continue
		if parsing_organism:
			organism = organism + list(map(lambda s: s.strip(), line.split(';')))
		
		if '/organelle="mitochondrion"' in line: # organelle condition
			try:
				ind = organism.index("Metazoa") # taxa condition
			except:
				continue
			else:
				to_be_written = True
		
		if "mitochondrial" in definition.lower() or "mitochondrion" in definition.lower(): # organelle condition for the case '/organelle="mitochondrion"' is missing in the reference
			try:
				ind = organism.index("Metazoa") # taxa condition
			except:
				continue
			else:
				to_be_written = True
		
		# for marine taxa we include some nuclear genes
		if "Porifera" in organism or "Crustacea" in organism or "Chondrichthyes" in organism or "Actinopterygii" in organism or "Sarcopterygii" in organism: # marine taxa condition
			gene_search = definition.lower()
			gene_search = gene_search.replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("("," ").replace(")"," ").replace("-"," ").replace("_"," ")
			gene_search = " " + gene_search + " "
			for n in range(0,gene_length):
				name = gene_names[n]
				if name in gene_search : # condition of belonging to the list of nuclear genes we selected
					to_be_written = True
		
		


	# to write down the last reference of the opened file if it matches the conditions
	if to_be_written: 
		outf.write('\n'+data)

	fin.close()


import glob
outf = open('animal_references_mitochondrial_nuclear_seq.txt','w') # create output file where selected references will be written
file_list=glob.glob('*.seq') # list files ending with ".seq" present in the folder where the script is
cnt = 0
for file in file_list:
	select_genbank_references(file, outf)
	cnt += 1
	print(file + " " + str((cnt*100)/len(file_list)) + " %")
outf.close()


### to print the execution time of the program
def endlog():
    end = time()
    elapsed = end-start
    log("End Program", secondsToStr(elapsed))
endlog()


