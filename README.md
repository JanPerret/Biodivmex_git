# Biodivmex_git

Code for the following paper:

Perret, J., Cobelli, O., Taudière, A., Andrieu, J., Aumeeruddy-Thomas, Y., Ben Souissi, J., Besnard, G., Casazza, G., Crochet, P.-A., Decaëns, T., Denis, F., Geniez, P., Loizides, M., Médail, F., Pasqualini, V., Speciale, C., Battesti, V., Chevaldonné, P., Lejeusne, C., & Richard, F. (2023). Time to refine the geography of biodiversity hotspots by integrating molecular data: The Mediterranean Basin as a case study. Biological Conservation, 284, 110162. https://doi.org/10.1016/j.biocon.2023.110162

The data are stored separately because they are too voluminous. Please contact me if something goes wrong with the code, or for any question.

Mail: [jan.perret\@cefe.cnrs.fr](mailto:jan.perret@cefe.cnrs.fr) or [jan.perret\@gmail.com](mailto:jan.perret@gmail.com)

## Steps you need to follow to run the code

1.  Download the current folder.

2.  Download the ZIP file "Biodivmex_data.zip" from the Zenodo link below, and unzip it in the root directory.

Zenodo link: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7341641.svg)](https://doi.org/10.5281/zenodo.7341641)

With these files you will be able to run all the analysis and make the figures presented in the article by executing the script "make.R" located in the root directory.

3.  The data filtering and cleaning process was done with Python programs (stored in the ./Python directory). If you want to run again this process, download the ZIP files below. How these programs work is described in the sections below.

For Web of Science (251 Mo): [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7341655.svg)](https://doi.org/10.5281/zenodo.7341655)

For Genbank (3.1 Go): [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7341767.svg)](https://doi.org/10.5281/zenodo.7341767)


# Web-of-Science-metadata-extractor

### General purpose

These programs extract some metadata from the .txt export files of Web of Science (WOS), with a final output being a table with one line per WOS reference and the following columns : WOS accession number, language, document type, publication year, corresponding author(s) nationality(ies) and the country(ies) where the fieldwork was done.

It was made with the export files of the research criteria "mediterran\*" in the Topic field of Web of Science, and has not been tested for other research criteria. Thus, it may need some modification in order to be used for other files.

The only import needed is the "glob" module that permit to list the files name in a folder. In addition different modules are needed if you choose to use the same way to measure the execution time but it is not necessary.

### Description of each script

1_supp_2\_first_lines_all_WOS_txt_files_and_merge_them.py

This program deletes the first 2 lines of every file ending with ".txt" that is located in the folder where the script is, and merge them into a single plain text file. The first 2 lines correspond to WOS export information and could enter in conflict with the following programs so I chose to get rid of them.

2_WOS_file_parser_v2.py

This program opens the plain text file whose name is given by the user at the beginning of the execution of the program (typically the output file of the precedent program, or a unique WOS export file in plain text format). The file is scanned line by line and some information are stored in objects and written into a .txt output file (a comma-separated table). The information extracted in this version are : document type, language, publisher, publication year, DOI, WOS accession number, research areas, author names, title, author keywords, keywords plus, abstract, address of the reprint author.

3_WOS_table_simplifier_v15.py

This program is meant to be used on the output file of the WOS_file_parser_v2.py (it splits every line of the input file by commas, so it has to be a comma-separated table). For each line of the table in input, it keeps only the following fields : WOS accession number, language, document type, publication year, and the country(ies) where the fieldwork was done. In addition it performs a country name search in the address of the corresponding author(s) in order to add a column with the "nationality of the author", and a search of the Mediterranean countries names in the following fields to affiliate the reference to different studied countries : title, abstract, author keywords, keywords plus, research areas. It also performs a research of certain keywords associated with different taxa to affiliate each reference to the studied taxa. For some taxa an external species keywords and name list which can not be communicated was used, but for other taxa a simple short list of keywords was used. These keywords are present in the script. The possible country names are taken from the "WOS_mediterranean_country_v4.txt" file.

name_list_WOS_country_v3.txt

A list of the possible country names met in the address field of WOS export files. I added to the list some of the common typing/filling errors that I found (for example "U.K" instead of "UK", or "Belgique" instead of "Belgium"). Each country name is associated to the "final" chosen country name.

name_list_WOS_mediterranean_country_v5.txt

Same list as "name_list_WOS_country_v3.txt" but only with the Mediterranean countries.

### Program Requirements

-   python 3.7
-   import glob module

### To download WOS files

Go to the following address : <https://apps.webofknowledge.com/>

Make a research following your own criteria. Click on the drop-down menu located on the top of the results of the research, and choose "Save to Other File Formats". On the resulting window, choose the number of records you want to save (Only 500 records at a time can be downloaded !), choose "Full Record and Cited References" for the Record Content, and "Plain Text" for the File Format.

# NCBI-GenBank-metadata-extractor

### General purpose

This suite of programs extracts some meta-data from the .seq files of NCBI GenBank, with a final output being a table with one line per GenBank reference and the following columns : accession number, species, submission year, sequencer nationality, sample origin, latitude and longitude of the origin of the sample. It was made with the files of the 248 release from February 2022, and for animal, fungi and plants files only. Thus, it may need some modification in order to be used for other files.

It was first based on dewshr's NCBI-GenBank-file-parser : <https://github.com/dewshr/NCBI-GenBank-file-parser>

But because I had to parse a large number of files, I changed the approach into a step by step way in order to have intermediate result files decreasing in size at each step. I choose a line by line scanning way of the files that permit to gain in execution time. The only module to import is the "glob" module that permit to list the files name in a folder. In addition different modules are needed if you choose to use the same way of measuring the execution time (first 30 lines of each script) but it is not necessary.

### Description of each script

1_supp_10_first_lines_all\_.seq_files.py

This program deletes the first 10 lines of every file ending with ".seq" that is located in the folder where the script is. These first 10 lines correspond to the GenBank release information and could enter in conflict with the following programs so I chose to get rid of them.

2_fungi_Genbank_references_selection.py ; 2_animal_Genbank_references_selection.py ; 2_plant_Genbank_references_selection.py

These programs scan all the .seq files in the working directory line by line and put in one .txt file all the references meeting the specified conditions.

3_Genbank_file_parser_v10.py

This program opens the file whose name is given by the user at the beginning of the execution of the program (typically the output file of the "...\_Genbank_references_selection.py" programs, or any other file that has the same structure than the .seq GenBank files). The file is scanned line by line and some information are stored in objects and written into a .txt output file (a comma-separated table). The information extracted in this version are : accession number, species name, submission year, latitude and longitude of the origin of the sample, origin country of the sample, definition of the reference (= the title), molecule type, submission date, address of the lab who did the sequencing, taxonomal hierarchy of the species.

4_Genbank_simple_table_maker_v11.py

This program is meant to be used on the output file of the Genbank_file_parser_v10.py (it splits every line of the input file by commas, so it has to be a comma-separated table). For each line of the table in input, it keeps only the following fields : the accession number, the species name, the submission year, the origin country of the sample and the lat/lon of the origin place of the sample. In addition to that it performs a country name search in the address of the lab where the sequencing was done in order to add a column with the "nationality of the sequencer". The possible country names are taken from the "GenBank_country_v2.txt" file, and some multiple assignation errors are corrected as indicated in the "country_assignation_errors.txt" file (see below).

GenBank_country_v2.txt

A list of the possible country names met in the address field of GenBank. The list is based on the official country list for the "/country" qualifier found here : <https://www.ncbi.nlm.nih.gov/genbank/collab/country/>

I added to the list the common typing/filling errors that I found (for example "U.K" instead of "UK", or "Belgique" instead of "Belgium"). Each country name is associated to the "final" chosen country name.

country_assignation_errors.txt

A list of the common country multiple-assignation errors I saw. For example, for the EU512160 reference, the address is : "Biology, New Mexico State University, Sweet and Horseshoe, Foster Hall, Rm 201, Las Cruces, NM 88001, USA" So it will be assigned to 2 countries : USA and Mexico. As a correction, every reference associated to these two countries will me corrected to "USA" only. Of course for new search criteria another you may need to make your own multiple-assignation correction list.

### Program Requirements

-   python 3.7
-   import glob module

### To download NCBI Genbank files

<ftp://ftp.ncbi.nlm.nih.gov/genbank/>

# Funding

These programs were written while I worked at the Center for Functional and Evolutionary Ecology in Montpellier, France : <https://www.cefe.cnrs.fr/fr/>

And funded by the Biodivmex program : <http://biodivmex.imbe.fr/>

# License for WOS and Genbank metadata extractors

    Web-of-Science-metadata-extractor is a program made to extract 
    some metadata from WOS export files in plain text format.
    Copyright (C) 2019 Jan Perret

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <https://www.gnu.org/licenses/>.
