# Groups Analysis Code for 'Performance of Crisis Standards of Care Guidelines in a National Cohort of Critically Ill COVID-19 Patients"

This code is for the groups analysis

Running Instructions:

Rscript Groups_Analysis.R [input file] [group size] [number of iterations] [size of each bootstrap] [final output filename]

Example:
Rscript Groups_Analysis.R input.txt 2 100 1000 final.txt


*** Note: Code was slightly modified to do analysis by Race ***

INPUT DESCRIPTIONS:

1. Input File: see "Example Input" for columns and format 
2. Group size: The size of each group (the paper did groups of 2 and groups of 5)
3. Number of Iterations: The total n for the distribution of percentages (the paper used 100)
4. Size of each Bootstrap: the number of random groups in each iteration to develop the percentages
5. Final output: name of final output

FINAL OUTPUT FORMATTING:

1. Column 1: Type of Algorithm
2. Column 2: Mean percentage of decisions made without lottery
3. Column 3: 95% CI for percentage of decisions made without lottery
4. Column 4: Mean percentage of non-lottery decisions that chose a surviving patient
5. Column 5: 95% CI for percentage of non-lottery decisions that chose a surviving patient
6. Colunn 6: Mean percentage of all decisions that chose a surviving patient
7. Colunn 7: 95% CI  of all decisions that chose a surviving patient

