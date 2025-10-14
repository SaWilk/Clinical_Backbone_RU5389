Purpose of the code:

The present repository contains code that is intended to clean, preprocess monitor and ultimately analyze basic features of the backbone data collected for the RU5389. 
The main purpose of the repository is to ensure transparency, the code is currently not optimized for portability since the raw data is not accessible anyway.

As a member of the RU5389 you may use any part of the code in any way you like. If you use an entire script or function as it is without massive changes it would be nice to credit me (Saskia Wilken)

Expected input/output:

Expected input is the raw data output from the limesurvey and psytoolkit websites for the backbone questionnaires. the output is so far:
- cleaned and separated project data that is shared within the RU
- a graph that monitors progress with data collection
- SOON: descriptive analysis results

Dependencies or assumptions:

Only R and RStudio. The scripts were written on Windows 11, RStudio version 2025.09.0, R version 4.5.1.

Folder structure: 

The main folder contains the major scripts that fulfill the functions of the repository. Within this there are subfolders:
\functions contains functions used in the scripts
\information contains information both to inform the scripts and also users
\tests contains small helper scripts that fulfill a specific, well defined function outside the main scope of the repo. 

Note: confidential information is not shared in the repository. script Prep_01 depends on this information and is thus not portable. If you need to execute the script on your machine, contact Saskia Wilken to ask for the necessary tables.

Contact info for questions:

saskia.wilken@uni-hamburg.de or saskia.a.wilken@gmail.com
