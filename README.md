# MB2-FinalProject

This code was written for the final submission of the course: 'Introduction to programming and statistics for remote sensing and GIS' (04-GEO-MB2) for the EAGLE master at the university of Würzburg in April 2022.\\

Land cover change due to coal mining in the Adoro coal mine, Borneo, Indonesia is analysed using three MODIS scenes from 2002, 2011 and 2021 with a resolution of 500m.
The extent of the mine for each year is determined with an unsupervised classification using the 'Hartigan-Wong' algorithm applied on calculated indices NDVI, NDWI and BSI. \\
Changes between these years are detected with both, a postclassification unsing the classification results as well as a change vector analysis applied on calculated tasseled cap tranformation (brighness and greeness of the scenes). \\
The results will be visualized with the ggplot package and animated .gif files. 
