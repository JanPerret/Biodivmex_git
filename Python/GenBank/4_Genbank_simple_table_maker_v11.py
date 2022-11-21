
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


### reading the table resulting from the GenBank parser
table_file_name = ""
table_file_name = str(input("Enter your table file name (including the file extension) : "))


def Simple_table_maker(table_file_name, output_table):
	fin = open(table_file_name, "r")
	
	# loading list of possible country names to search in the sequencer address
	country_index = open("name_list_GenBank_country_v2.txt", "r")
	country_names = []
	country_finals = []
	country_line_list = []
	for line in country_index:
		country_line_list = line.split(",",1)
		country_names.append(" " + country_line_list[0].lower().replace('\n','') + " ") # makes a list of the possible country names with a space before and after each name
		country_finals.append(country_line_list[1].replace('\n','')) # makes a list of the 'final' country names I choose associated with the country names of the precedent list
	country_length = len(country_names)
	address_search = ""
	name_final = ""

	# loading list of country assignation errors (personally verified one by one)
	errors = open("name_list_country_assignation_errors_v2.txt", "r")
	error_names = []
	error_correct = []
	errors_line_list = []
	for line in errors:
		errors_line_list = line.split(",",1)
		error_names.append(errors_line_list[0].replace('\n','')) # makes a list of the common multiple-assignation errors that will be automatically corrected
		error_correct.append(errors_line_list[1].replace('\n','')) # makes a list of the correct country assignations associated with the precedent list
	error_length = len(error_names)
	error = ""
	correct = ""
	
	# loading list of mediterranean islands names
	island_index = open("name_list_mediterranean_islands.txt", "r")
	island_line_list = []
	island_countries = []
	island_names = []
	island_finals = []
	for line in island_index:
		island_line_list = line.split(",",2)
		island_countries.append(island_line_list[0].replace('\n','')) # makes a list of the nationalities of the islands
		island_names.append(" " + island_line_list[1].lower().replace('\n','') + " ") # makes a list of the possible island names with a space before and after each name
		island_finals.append(island_line_list[2].replace('\n','')) # makes a list of the 'final' island names associated with the island names of the precedent list
	island_length = len(island_names)
	island_country = ""
	island_name = ""
	island_final = ""
	supp_geo_infos_search = ""
	
	# loading list of the extra-mediterranean territories of the countries from the mediterranean basin
	territory_index = open("name_list_mediterranean_countries_external_territories.txt", "r")
	territory_country = []
	territory_name_list = []
	territory_line_list = []
	for line in territory_index:
		territory_line_list = line.split(",",1)
		territory_country.append(territory_line_list[0].replace('\n','')) # makes a list of the countries of attachment of the territories
		territory_name_list.append(" " + territory_line_list[1].lower().replace('\n','') + " ") # makes a list of the territory names to search in the supplementary geographic information (=supp_geo_infos), with a space before and after each name
	territory_length = len(territory_country)
	attachment_country = ""
	territory_name = ""
	supp_geo_infos_search = ""
	
	
	###### loading name lists of marine taxa
	# loading the FISH species name list
	fish = open("name_list_fish_FishBase_without_keywords.txt", "r")
	fish_line = []
	fish_names = []
	for line in fish:
		fish_line = line
		fish_names.append(" " + fish_line.lower().replace('\n','') + " ")
	fish_length = len(fish_names)
	fish = ""

	# loading the porifera species name list
	porifera = open("name_list_porifera_WPDB_v3_with_synonyms_marine_province_Grenier2018_without_keywords.txt", "r")
	porifera_line = []
	porifera_names = []
	for line in porifera:
		porifera_line = line
		porifera_names.append(" " + porifera_line.lower().replace('\n','') + " ")
	porifera_length = len(porifera_names)
	porifera = ""

	# loading the CRUSTACEA species name list
	crustacea = open("name_list_crustacea_v4_WoRMS_IHO_sea_area.txt", "r")
	crustacea_line = []
	crustacea_names = []
	for line in crustacea:
		crustacea_line = line
		crustacea_names.append(" " + crustacea_line.lower().replace('\n','') + " ")
	crustacea_length = len(crustacea_names)
	crustacea = ""

	# loading the TREES species name list
	tree = open("name_list_trees_cyprus_version.txt", "r")
	tree_line = []
	tree_names = []
	for line in tree:
		tree_line = line
		tree_names.append(" " + tree_line.lower().replace('\n','') + " ")
	tree_length = len(tree_names)
	tree = ""

	###### loading animal mitochondrial gene names
	gene_index = open("name_list_animal_mitochondrial_genes_v3.txt", "r")
	gene_names = []
	gene_finals = []
	gene_line_list = []
	for line in gene_index:
		gene_line_list = line.split(",",1)
		gene_names.append(" " + gene_line_list[0].lower().replace('\n','') + " ") # makes a list of the possible gene names with a space before and after each name
		gene_finals.append(gene_line_list[1].replace('\n','')) # makes a list of the 'final' gene names I choose associated with the gene names of the precedent list
	gene_length = len(gene_names)
	gene_search = ""
	gene_final = ""


	# object initialization
	access_num = ""
	definition = ""
	species = ""
	organism = ""
	sub_date = ""
	year = ""
	address = ""
	address_search = ""
	mol_type = ""
	country = ""
	supp_geo_infos = ""
	name = ""
	sequencer_nationality = ""
	gene = ""
	gene_prev = ""
	taxa = ""
	species_level = ""
	species_list = ""
	line_list = []
	ref_infos = ""
	line_cnt = 1;


	### This loop reads a comma-separated table file line by line
	### for each line, only a certain amount of data is transcribed in the output file :
	### the accession number, the species name, the submission year,
	### the nationality of the sequencer lab, and the origin country of the sample.
	### To go from the address to the nationality of the sequencing lab, a research of the country names contained 
	### in the "GenBank_country_v2.txt" is made.
	### Some multiple-assignation errors (contained in the "country_assignation_errors.txt" file) are directly corrected.
	### This correction list is arbitrary and corresponds to the errors I personally corrected by checking the address of every
	### multiple-assignation case I had for my use of these programs (release 228, COI+animals / ITS+fungi / MatK+plant / rbcL+plant selection criteria).
	### For every new type of reference selection criteria you may need to establish your own multiple-assignation correction list,
	### as well as your own country list because I took into account the most common typing errors in country names that I saw for my search criteria
	### but there might be other errors for other researches.
	first_line = fin.readline() # to not take into account the first line with the columns headings
	for line in fin:
		line_cnt += 1
		line_list = line.split(",")
		if len(line_list) < 9:
			print("Error in line " + str(line_cnt) + ": " + str(line_list))
			continue
		access_num = str(line_list[0])
		species = str(line_list[1])
		year = str(line_list[2])
		country = str(line_list[4])
		definition = str(line_list[5])
		mol_type = str(line_list[6])
		address = str(line_list[8])
		organism = str(line_list[9])
		if mol_type != "genomic DNA":
			continue
		if ":" in country:
			supp_geo_infos = country.split(":",1)[1].replace(";"," ").strip()
			country = country.split(":",1)[0].strip()
		
		# To re-assign the biggest islands to their own name instead of their nationality
		if supp_geo_infos:
			supp_geo_infos_search = supp_geo_infos.lower()
			supp_geo_infos_search = supp_geo_infos_search.replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("("," ").replace(")"," ").replace("-"," ").replace("_"," ")
			supp_geo_infos_search = " " + supp_geo_infos_search + " "
			for n in range(0,island_length):
				island_country = island_countries[n]
				island_name = island_names[n]
				island_final = island_finals[n]
				if country == island_country and island_name in supp_geo_infos_search:
					country = island_final
		
		# To eliminate the sequences collected in extra-mediterranean territories
		if supp_geo_infos:
			supp_geo_infos_search = supp_geo_infos.lower()
			supp_geo_infos_search = supp_geo_infos_search.replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("("," ").replace(")"," ").replace("-"," ").replace("_"," ")
			supp_geo_infos_search = " " + supp_geo_infos_search + " "
			for n in range(0,territory_length):
				attachment_country = territory_country[n]
				territory_name = territory_name_list[n]
				if country == attachment_country and territory_name in supp_geo_infos_search:
					access_num = ""
					species = ""
					year = ""
					country = ""
					definition = ""
					mol_type = ""
					address = ""
					organism = ""
					supp_geo_infos = ""
					supp_geo_infos_search = ""

		# To extract the nationality of the sequencer
		if address:
			address_search = address.lower()
			address_search = address_search.replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("("," ").replace(")"," ").replace("-"," ").replace("_"," ")
			address_search = " " + address_search + " "
			for n in range(0,country_length):
				name = country_names[n]
				name_final = country_finals[n]
				if name in address_search: # assignation of each country name found in the address to the "sequencer_nationality" object
					sequencer_nationality = sequencer_nationality + ' ' + name_final
		sequencer_nationality = sequencer_nationality.strip()
		if sequencer_nationality:
			for n in range(0,error_length):
				error = error_names[n]
				correct = error_correct[n]
				if error in sequencer_nationality: # if a common country multiple-assignation case is found, it is corrected like it is written in the "country_assignation_errors.txt" file
					sequencer_nationality = correct

		# To extract gene(s) name(s)
		# For plants and Fungi :
		if definition != "" and ("Embryophyta;" in organism or "Fungi;" in organism): # if there is something in "definition" and organism is a fungi or a plant
			if "internal transcribed spacer" in definition.lower() or "ITS" in definition:
				gene = "ITS"
			elif "maturase k" in definition.lower() or "matk" in definition.lower():
				gene = "MatK"
			elif "ribulose bisphosphate carboxylase" in definition.lower() or "rbcl" in definition.lower():
				gene = "rbcL"
			elif "ribulose" in definition.lower() and "bisphosphate" in definition.lower() and "carboxylase" in definition.lower() : # 2nd rbcL condition (because this gene name if often in multiple parts or with dashes)
				gene = "rbcL"
		# For Metazoa :
		if definition != "" and "Metazoa;" in organism:
			gene_search = definition.lower()
			gene_search = gene_search.replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("("," ").replace(")"," ").replace("-"," ").replace("_"," ")
			gene_search = " " + gene_search + " "
			for n in range(0,gene_length):
				name = gene_names[n]
				name_final = gene_finals[n]
				if name in gene_search : # assignation of each gene name found in the "definition" object
					gene_prev += ' ' + name_final
					gene_prev = gene_prev.strip()
				if gene_prev:
					if not gene_prev in gene: # so we don't get multiple assignations in case multiple versions of the same gene name are written in the definition
						gene += gene_prev + " // "
				gene_prev = ""
			gene = gene.strip()
			gene = gene[:-3]
		
		# Taxa assignation
		if organism:
			if "Amphibia;" in organism:
				taxa = "Amphibian"
			elif "Crocodylia;" in organism:
				taxa = "Reptile"
			elif "Lepidosauria;" in organism:
				taxa = "Reptile"
			elif "Testudines;" in organism:
				taxa = "Reptile"
			elif "Aves;" in organism:
				taxa = "Bird"
			elif "Mammalia;" in organism:
				taxa = "Mammal"
			elif "Coleoptera;" in organism:
				taxa = "Coleoptera"
			elif "Papilionoidea;" in organism:
				taxa = "Papilionoidea"
			elif "Lumbricina;" in organism:
				taxa = "Lumbricina"
			elif "Fungi;" in organism:
				taxa = "Fungi"
			elif "Embryophyta;" in organism:
				taxa = "Terrestrial plant"
			elif "Porifera;" in organism:
				taxa = "Porifera"
			elif "Crustacea;" in organism:
				taxa = "Crustacea"
			elif "Chondrichthyes;" in organism: # cartilaginous fishes
				taxa = "Fish"
			elif "Actinopterygii;" in organism: # ray-finned fishes (included in the Osteichthyes, or "bony fishes")
				taxa = "Fish"
			elif "Dipnoi;" in organism: # lungfishes fishes (included in the Osteichthyes, or "bony fishes")
				taxa = "Fish"
			elif "Coelacanthimorpha;" in organism: # coelacanths
				taxa = "Fish"
		
		if organism: # separate condition from the precedent in order to replace taxa="Mammal" by taxa="Marine mammal" if the sequence comes from a marine mammal
			if "Cetacea;" in organism:
				taxa = "Marine mammal"
			elif "Sirenia;" in organism:
				taxa = "Marine mammal"
			elif "Otariidae;" in organism:
				taxa = "Marine mammal"
			elif "Odobenidae;" in organism:
				taxa = "Marine mammal"
			elif "Phocidae;" in organism:
				taxa = "Marine mammal"
		
		# To eliminate the sequences collected in the Red Sea for marine taxa
		if taxa == "Porifera" or taxa == "Crustacea" or taxa == "Fish":
			if supp_geo_infos:
				supp_geo_infos_search = supp_geo_infos.lower()
				supp_geo_infos_search = supp_geo_infos_search.replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("("," ").replace(")"," ").replace("-"," ").replace("_"," ")
				supp_geo_infos_search = " " + supp_geo_infos_search + " "
				if " red sea " in supp_geo_infos_search:
					country = ""

		
		############################################## AFFILIATION TAXONS MARINS via les listes de noms d'especes pour voir ce que ca donne
		if species:
			species_search = species.replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("("," ").replace(")"," ").replace("-"," ").replace("_"," ")
			species_search = species_search.replace("	"," ")
			species_search = " "+species_search.lower()+" "
			
			if taxa == "Fish":
				for n in range(0,fish_length):
					name = fish_names[n]
					name_final = "fish"
					if name in species_search:
						if not name_final in fish:
							fish += ' ' + name_final
							fish = fish.strip()
			
			if taxa == "Porifera":
				for n in range(0,porifera_length):
					name = porifera_names[n]
					name_final = "porifera"
					if name in species_search:
						if not name_final in porifera:
							porifera += ' ' + name_final
							porifera = porifera.strip()
			
			if taxa == "Crustacea":
				for n in range(0,crustacea_length):
					name = crustacea_names[n]
					name_final = "crustacea"
					if name in species_search:
						if not name_final in crustacea:
							crustacea += ' ' + name_final
							crustacea = crustacea.strip()
							
			if taxa == "Terrestrial plant":
				for n in range(0,tree_length):
						name = tree_names[n]
						name_final = "tree"
						if name in species_search:
							if not name_final in tree:
								tree += ' ' + name_final
								tree = tree.strip()
		
			species_search = ""
		##############################################
		
		if species:
			species_list = species.split(" ")
			if "[" in species :
				species_level = ""
			elif " f. sp." in species :
				species_level = species.split(" ")[0:2] # this index selects the first 2 elements of a list
				species_level = " ".join(species_level) # to convert the list into a string object
			elif " f." in species :
				species_level = species.split(" ")[0:2]
				species_level = " ".join(species_level)
			elif " sp." in species or " sp " in species :
				species_level = ""
			elif " nr." in species or " nr " in species :
				species_level = ""
			elif "cf." in species or " cf " in species : # no space before the expression with the point because some records are written like "cf. genus species"
				species_level = ""
			elif "aff." in species or " aff " in species : # no space for the same reason
				species_level = ""
			elif " group" in species.lower() or " subgroup" in species.lower() :
				species_level = ""
			elif " var." in species or " var " in species :
				species_level = species.split(" ")[0:2]
				species_level = " ".join(species_level)
			elif " subsp." in species or " subsp " in species :
				species_level = species.split(" ")[0:2]
				species_level = " ".join(species_level)
			elif " hybrid" in species.lower() :
				species_level = "hybrid"
			elif " x " in species :
				species_level = "hybrid"
			elif len(species_list) == 4 and len(species_list[3]) == 4 and species_list[3].isdigit() : # if the species field is in this format : "Stellaria chinensis Regel 1862"
				species_level = species.split(" ")[0:2]
				species_level = " ".join(species_level)
			elif len(species_list) == 4 and species_list[2] == "form" : # if the species field is in this format : "Corbicula javanica form B"
				species_level = species.split(" ")[0:2]
				species_level = " ".join(species_level)
			elif len(species_list) == 3 :
				species_level = species.split(" ")[0:2]
				species_level = " ".join(species_level)
			elif len(species_list) == 2 :
				species_level = species
			else:
				species_level = "BUG"



		ref_infos = access_num+','+mol_type+','+taxa+','+species+','+gene+','+year+','+sequencer_nationality+','+country+','+supp_geo_infos+','+species_level+','+fish+','+porifera+','+crustacea+','+tree
		
		access_num = ""
		mol_type = ""
		species = ""
		year = ""
		country = ""
		supp_geo_infos = ""
		definition = ""
		sequencer_nationality = ""
		address = ""
		address_search = ""
		supp_geo_infos_search = ""
		gene = ""
		gene_prev = ""
		organism = ""
		taxa = ""
		species_level = ""
		species_list = ""
		
		####################################
		fish = ""
		porifera = ""
		crustacea = ""
		tree = ""
		
		if ref_infos != ",,,,,,,,,,,,,":
				output_table.write('\n'+ref_infos)


output_table = open('SIMPLE_v11_'+table_file_name,'w') # opening a file to write the output
output_table.write('access_num'+','+'mol_type'+','+'taxa'+','+'species'+','+'gene'+','+'year'+','+'sequencer_nationality'+','+'sample_origin'+','+'supp_origin_infos'+','+'species_level'+','+'fish'+','+'porifera'+','+'crustacea'+','+'tree')
Simple_table_maker(table_file_name, output_table)
output_table.close()


### to print the execution time of the program
def endlog():
    end = time()
    elapsed = end-start
    log("End Program", secondsToStr(elapsed))
endlog()
