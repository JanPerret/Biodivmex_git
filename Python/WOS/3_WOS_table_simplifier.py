
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


### reading the table resulting from the WOS_file_parser_v2
table_file_name = ""
table_file_name = str(input("Enter your comma-separated table file name (including the file extension) : "))


def Simple_WOS_table_maker(table_file_name, output_table):
	fin = open(table_file_name, "r")
	
	# loading the WORLD COUNTRY name list
	country_index = open("name_list_WOS_country_v3.txt", "r")
	country_names = []
	country_finals = []
	country_line_list = []
	for line in country_index:
		country_line_list = line.split(",",1)
		country_names.append(" " + country_line_list[0].lower().replace('\n','') + " ") # makes a list of the possible country names with a space before and after each name
		country_finals.append(country_line_list[1].replace('\n','')) # makes a list of the 'final' country names I choose associated with the country names of the precedent list
	length = len(country_names)
	address_search = ""
	name_final = ""

	# loading the MEDITERRANEAN COUNTRY name list
	fw_country_index = open("name_list_WOS_mediterranean_country_v5.txt", "r")
	fw_country_names = []
	fw_country_finals = []
	fw_country_line_list = []
	for line in fw_country_index:
		fw_country_line_list = line.split(",",1)
		fw_country_names.append(" " + fw_country_line_list[0].lower().replace('\n','') + " ")
		fw_country_finals.append(fw_country_line_list[1].replace('\n',''))
	fw_length = len(fw_country_names)
	fw_country = ""
	fw_country_tot = ""
	
	# loading the OTHER MEDITERRANEAN REGIONS name list
	other_regions_index = open("name_list_other_mediterranean_regions.txt", "r")
	other_regions_line = ""
	other_regions = []
	for line in other_regions_index:
		other_regions_line = line
		other_regions.append(" " + other_regions_line.lower().replace('\n','') + " ")
	other_regions_length = len(other_regions)
	outside_med = ""
	
	# loading the MARINE REGIONS name list
	marine_region_index = open("name_list_med_marine_regions_v3.txt", "r")
	marine_region_names = []
	marine_region_finals = []
	marine_region_line_list = []
	for line in marine_region_index:
		marine_region_line_list = line.split(",",1)
		marine_region_names.append(" " + marine_region_line_list[0].lower().replace('\n','') + " ")
		marine_region_finals.append(marine_region_line_list[1].replace('\n',''))
	marine_region_length = len(marine_region_names)
	marine_region = ""
	marine_region_tot = ""
	
	# loading the AMPHIBIAN species name list
	amphibian_index = open("name_list_amphibian_Geniez_v5.txt", "r")
	amphibian_line = ""
	amphibian_names = []
	for line in amphibian_index:
		amphibian_line = line
		amphibian_names.append(" " + amphibian_line.lower().replace('\n','') + " ") # list of the possible amphibian species names with a space before and after each name
	amphibian_length = len(amphibian_names)
	amphibian = ""
	
	# loading the REPTILE species name list
	reptile_index = open("name_list_reptiles_Geniez_v5.txt", "r")
	reptile_line = ""
	reptile_names = []
	for line in reptile_index:
		reptile_line = line
		reptile_names.append(" " + reptile_line.lower().replace('\n','') + " ")
	reptile_length = len(reptile_names)
	reptile = ""
	
	# loading the BIRD species name list
	bird_index = open("name_list_bird_AVIBASE_v3_sci_eng_keywords.txt", "r")
	bird_line = ""
	bird_names = []
	for line in bird_index:
		bird_line = line
		bird_names.append(" " + bird_line.lower().replace('\n','').replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("-"," ") + " ")
	bird_length = len(bird_names)
	bird = ""
	
	# loading the TERRESTRIAL PLANTS keyword list
	plant = open("name_list_terrestrial_plants.txt", "r")
	plant_line = []
	plant_names = []
	for line in plant:
		plant_line = line
		plant_names.append(" " + plant_line.lower().replace('\n','') + " ")
	plant_length = len(plant_names)
	plant = ""
	# and loading plant exclusion criteria
	plant_exclusion_names = [" alga "," algae "," posidonia "]
	plant_exclusion_names_length = len(plant_exclusion_names)

	# loading the FUNGI species name list
	fungi = open("name_list_fungi.txt", "r")
	fungi_line = []
	fungi_names = []
	for line in fungi:
		fungi_line = line
		fungi_names.append(" " + fungi_line.lower().replace('\n','') + " ")
	fungi_length = len(fungi_names)
	fungi = ""
	
	# loading the COLEOPTERA species name list
	coleoptera = open("name_list_coleoptera_v2.txt", "r")
	coleoptera_line = []
	coleoptera_names = []
	for line in coleoptera:
		coleoptera_line = line
		coleoptera_names.append(" " + coleoptera_line.lower().replace('\n','') + " ")
	coleoptera_length = len(coleoptera_names)
	coleoptera = ""

	# loading the PAPILIONOIDEA species name list
	papilionoidea = open("name_list_papilionoidea_v2.txt", "r")
	papilionoidea_line = []
	papilionoidea_names = []
	for line in papilionoidea:
		papilionoidea_line = line
		papilionoidea_names.append(" " + papilionoidea_line.lower().replace('\n','') + " ")
	papilionoidea_length = len(papilionoidea_names)
	papilionoidea = ""

	# loading the LUMBRICINA species name list
	lumbricina = open("name_list_lumbricina_v2.txt", "r")
	lumbricina_line = []
	lumbricina_names = []
	for line in lumbricina:
		lumbricina_line = line
		lumbricina_names.append(" " + lumbricina_line.lower().replace('\n','') + " ")
	lumbricina_length = len(lumbricina_names)
	lumbricina = ""
	
	# loading the TERRESTRIAL MAMMAL species name list
	mammal = open("name_list_terrestrial_mammals_v1.txt", "r")
	mammal_line = []
	mammal_names = []
	for line in mammal:
		mammal_line = line
		mammal_names.append(" " + mammal_line.lower().replace('\n','') + " ")
	mammal_length = len(mammal_names)
	mammal = ""
	
	# loading the MARINE MAMMAL species name list
	marine_mammal = open("name_list_marine_mammals_v1.txt", "r")
	marine_mammal_line = []
	marine_mammal_names = []
	for line in marine_mammal:
		marine_mammal_line = line
		marine_mammal_names.append(" " + marine_mammal_line.lower().replace('\n','') + " ")
	marine_mammal_length = len(marine_mammal_names)
	marine_mammal = ""
	
	# loading the FISH species name list
	fish = open("name_list_fish_FishBase_without_keywords.txt", "r")
	fish_line = []
	fish_names = []
	for line in fish:
		fish_line = line
		fish_names.append(" " + fish_line.lower().replace('\n','') + " ")
	fish_length = len(fish_names)
	fish = ""

	# loading the SPONGE species name list
	sponge = open("name_list_sponge_WPDB_v3_with_synonyms_marine_province_Grenier2018_and_keywords.txt", "r")
	sponge_line = []
	sponge_names = []
	for line in sponge:
		sponge_line = line
		sponge_names.append(" " + sponge_line.lower().replace('\n','') + " ")
	sponge_length = len(sponge_names)
	sponge = ""

	# loading the CRUSTACEA species name list
	crustacea = open("name_list_crustacea_v4_WoRMS_IHO_sea_area_with_keywords.txt", "r")
	crustacea_line = []
	crustacea_names = []
	for line in crustacea:
		crustacea_line = line
		crustacea_names.append(" " + crustacea_line.lower().replace('\n','') + " ")
	crustacea_length = len(crustacea_names)
	crustacea = ""

	# initialising the objects used in the loop
	doc_type = ""
	language = ""
	publisher = ""
	year = ""
	DOI = ""
	access_num = ""
	research_areas = ""
	author_names = ""
	title = ""
	auth_keywords = ""
	keywords_plus = ""
	abstract = ""
	address = ""
	name = ""
	author_nationality = ""
	author_nationality_tot = ""
	line_list = []
	search_mix = ""
	fw_country = ""
	fw_country_tot = ""
	outside_med = ""
	marine_region = ""
	marine_region_tot = ""
	amphibian = ""
	reptile = ""
	bird = ""
	mammal = ""
	marine_mammal = ""
	fish = ""
	sponge = ""
	crustacea = ""
	plant = ""
	fungi = ""
	coleoptera = ""
	papilionoidea = ""
	lumbricina = ""
	ref_infos = ""
	line_cnt = 1;

	### This loop reads a comma-separated table file line by line.
	### For each line, only a certain amount of data is transcribed in the output file :
	### the WOS accession number, the language, the document type, the publication year,
	### the corresponding author(s) nationality(ies), and the country(ies) where the fieldwork was done.	
	### To extract the nationality of the corresponding author(s) from their address,
	### a research of the country names contained in the "name_list_WOS_country_v3.txt" is made.
	### In addition it performs for each line a research of some keywords associated with
	### specific taxa to affiliate the publications to the taxa their talking about.
	### For amphibians, reptiles and birds, a species name list (which could not be communicated)
	### is loaded, and for the other taxa the used keywords are shown in the script below.
	first_line = fin.readline() # to not take into account the first line with the columns headers
	for line in fin:
		line_cnt += 1
		line_list = line.split(",")
		if len(line_list) != 13:
			print("Error in line " + str(line_cnt) + ": " + str(line_list))
			continue
		doc_type = str(line_list[0])
		language = str(line_list[1])
		publisher = str(line_list[2])
		year = str(line_list[3])
		DOI = str(line_list[4])
		access_num = str(line_list[5])
		research_areas = str(line_list[6])
		author_names = str(line_list[7])
		title = str(line_list[8])
		auth_keywords = str(line_list[9])
		keywords_plus = str(line_list[10])
		abstract = str(line_list[11])
		address = str(line_list[12])

		# if language == "English": # j'enleve cette condition car les articles dans d'autres langues ont aussi des titres et abstracts en anglais !
		search_mix = research_areas+" "+title+" "+auth_keywords+" "+abstract
		search_mix = search_mix.replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("("," ").replace(")"," ").replace("-"," ").replace("_"," ")
		search_mix = search_mix.replace("	"," ")
		search_mix = " "+search_mix.lower()+" "
		
		if not " mediterranean " in search_mix or " mediterran " in search_mix: # condition to exclude the articles which only have these words in the "keywords_plus" (automatically generated by WOS, include errors)
			doc_type = ""
			language = ""
			publisher = ""
			year = ""
			DOI = ""
			access_num = ""
			research_areas = ""
			author_names = ""
			title = ""
			auth_keywords = ""
			keywords_plus = ""
			abstract = ""
			address = ""
			search_mix = ""
			continue

		if address:
			address_list = address.split("(reprint author)")
			nlist = len(address_list)
										# species = species.split(".",1)[0]
			for i in range(0,nlist):
				address_search = address_list[i].lower()
				address_search = address_search.replace("."," ").replace(","," ").replace(";"," ").replace(":"," ").replace("'"," ").replace("("," ").replace(")"," ").replace("-"," ").replace("	"," ")
				address_search = " " + address_search + " "
				for n in range(0,length):
					name = country_names[n]
					name_final = country_finals[n]
					if name in address_search: # assignation of each country name found in the address to the "author_nationality" object
						author_nationality += ' ' + name_final
						author_nationality = author_nationality.strip()
					if author_nationality:
						if not author_nationality in author_nationality_tot: # so we don't get multiple assignations in case the multiple reprint authors are from the same country
							author_nationality_tot += author_nationality + " // " 
					author_nationality = ""
			author_nationality_tot = author_nationality_tot.strip()
			author_nationality_tot = author_nationality_tot[:-3]
			# if author_nationality_tot == "" and len(address_list[i]) > 2:
				# print("Assignation error in line "+str(line_cnt)+" : an address is present but no country was found. Address : "+address)
		
		for n in range(0,fw_length):
			name = fw_country_names[n]
			name_final = fw_country_finals[n]
			if name in search_mix: # assignation of each country name found in the search_mix object to the "fieldwork_country" object
				fw_country += ' ' + name_final
				fw_country = fw_country.strip()
			if fw_country:
				if not fw_country in fw_country_tot: # so we don't get multiple assignations in case the same country is found multiple times
					fw_country_tot += fw_country + " // "
			fw_country = ""
		fw_country_tot = fw_country_tot.strip()
		fw_country_tot = fw_country_tot[:-3]
		
		for n in range(0,marine_region_length):
			name = marine_region_names[n]
			name_final = marine_region_finals[n]
			if name in search_mix:
				marine_region += ' ' + name_final
				marine_region = marine_region.strip()
			if marine_region:
				if not marine_region in marine_region_tot:
					marine_region_tot += marine_region + " // "
			marine_region = ""
		marine_region_tot = marine_region_tot.strip()
		marine_region_tot = marine_region_tot[:-3]
		
		# filtering other mediterranean regions
		for n in range(0,other_regions_length):
			name = other_regions[n]
			name_final = "outside_med"
			if name in search_mix and fw_country_tot == "": # double condition : article will be considered "outside med" if presence of name other med climate region and no med basin country name (because then it can be a comparison article)
				if not name_final in outside_med:
					outside_med += ' ' + name_final
					outside_med = outside_med.strip()
		
		for n in range(0,amphibian_length):
			name = amphibian_names[n]
			name_final = "amphibian"
			if name in search_mix:
				if not name_final in amphibian:
					amphibian += ' ' + name_final
					amphibian = amphibian.strip()
					
		for n in range(0,reptile_length):
			name = reptile_names[n]
			name_final = "reptile"
			if name in search_mix:
				if not name_final in reptile:
					reptile += ' ' + name_final
					reptile = reptile.strip()

		for n in range(0,bird_length):
			name = bird_names[n]
			name_final = "bird"
			if name in search_mix:
				if not name_final in bird:
					bird += ' ' + name_final
					bird = bird.strip()

		for n in range(0,marine_mammal_length):
			name = marine_mammal_names[n]
			name_final = "marine_mammal"
			if name in search_mix:
				if not name_final in marine_mammal:
					marine_mammal += ' ' + name_final
					marine_mammal = marine_mammal.strip()
		
		if not marine_mammal:
			for n in range(0,mammal_length):
				name = mammal_names[n]
				name_final = "mammal"
				if name in search_mix:
					if not name_final in mammal:
						mammal += ' ' + name_final
						mammal = mammal.strip()
					
		for n in range(0,fish_length):
			name = fish_names[n]
			name_final = "fish"
			if name in search_mix:
				if not name_final in fish:
					fish += ' ' + name_final
					fish = fish.strip()
					
		for n in range(0,sponge_length):
			name = sponge_names[n]
			name_final = "sponge"
			if name in search_mix:
				if not name_final in sponge:
					sponge += ' ' + name_final
					sponge = sponge.strip()

		for n in range(0,crustacea_length):
			name = crustacea_names[n]
			name_final = "crustacea"
			if name in search_mix:
				if not name_final in crustacea:
					crustacea += ' ' + name_final
					crustacea = crustacea.strip()

		for n in range(0,plant_length):
			name = plant_names[n]
			name_final = "plant"
			if name in search_mix:
				for a in range(0,plant_exclusion_names_length):
					exclusion_name = plant_exclusion_names[a]
					if not exclusion_name in search_mix:
						if not name_final in plant:
							plant += ' ' + name_final
							plant = plant.strip()
		
		for n in range(0,fungi_length):
			name = fungi_names[n]
			name_final = "fungi"
			if name in search_mix:
				if not name_final in fungi:
					fungi += ' ' + name_final
					fungi = fungi.strip()
					
		for n in range(0,coleoptera_length):
			name = coleoptera_names[n]
			name_final = "coleoptera"
			if name in search_mix:
				if not name_final in coleoptera:
					coleoptera += ' ' + name_final
					coleoptera = coleoptera.strip()

		for n in range(0,papilionoidea_length):
			name = papilionoidea_names[n]
			name_final = "papilionoidea"
			if name in search_mix:
				if not name_final in papilionoidea:
					papilionoidea += ' ' + name_final
					papilionoidea = papilionoidea.strip()
		
		for n in range(0,lumbricina_length):
			name = lumbricina_names[n]
			name_final = "lumbricina"
			if name in search_mix:
				if not name_final in lumbricina:
					lumbricina += ' ' + name_final
					lumbricina = lumbricina.strip()


		ref_infos = access_num+','+language+','+doc_type+','+publisher+','+year+','+author_nationality_tot+','+fw_country_tot+','+marine_region_tot+','+outside_med+','+plant+','+fungi+','+amphibian+','+reptile+','+bird+','+mammal+','+fish+','+sponge+','+crustacea+','+coleoptera+','+papilionoidea+','+lumbricina

		doc_type = ""
		language = ""
		publisher = ""
		year = ""
		DOI = ""
		access_num = ""
		research_areas = ""
		author_names = ""
		title = ""
		auth_keywords = ""
		keywords_plus = ""
		abstract = ""
		address = ""
		name = ""
		name_final = ""
		author_nationality = ""
		author_nationality_tot = ""
		search_mix = ""
		fw_country = ""
		fw_country_tot = ""
		marine_region = ""
		marine_region_tot = ""
		outside_med = ""
		amphibian = ""
		reptile = ""
		bird = ""
		mammal = ""
		marine_mammal = ""
		fish = ""
		sponge = ""
		crustacea = ""
		plant = ""
		fungi = ""
		coleoptera = ""
		papilionoidea = ""
		lumbricina = ""

		output_table.write('\n'+ref_infos)


output_table = open('TEST_SIMPLE_v14_'+table_file_name,'w') # opening a file to write the output
# write the column headers
output_table.write('access_num'+','+'language'+','+'doc_type'+','+'publisher'+','+'year'+','+'author_nationality'+','+'fieldwork_country'+','+'marine_region'+','+'outside_med'+','+'plant'+','+'fungi'+','+'amphibian'+','+'reptile'+','+'bird'+','+'mammal'+','+'fish'+','+'sponge'+','+'crustacea'+','+'coleoptera'+','+'papilionoidea'+','+'lumbricina')
Simple_WOS_table_maker(table_file_name, output_table)
output_table.close()


### to print the execution time of the program
def endlog():
	end = time()
	elapsed = end-start
	log("End Program", secondsToStr(elapsed))
endlog()
