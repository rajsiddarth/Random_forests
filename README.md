# Random forests

## Dataset
The data set considered has the following variables
1. TARGET: DIE, LIVE 
2. AGE: 10, 20, 30, 40, 50, 60, 70, 80 
3. GENDER: male, female 
4. STEROID: no, yes 
5. ANTIVIRALS: no, yes 
6. FATIGUE: no, yes 
7. MALAISE: no, yes 
8. ANOREXIA: no, yes 
9. LIVER BIG: no, yes 
10. LIVER FIRM: no, yes 
11. SPLEEN PALPABLE: no, yes 
12. SPIDERS: no, yes 
13. ASCITES: no, yes 
14. VARICES: no, yes 
15. BILIRUBIN: 0.39, 0.80, 1.20, 2.00, 3.00, 4.00  
16. ALK PHOSPHATE: 33, 80, 120, 160, 200, 250 
17. SGOT: 13, 100, 200, 300, 400, 500, 
18. ALBUMIN: 2.1, 3.0, 3.8, 4.5, 5.0, 6.0 
19. PROTIME: 10, 20, 30, 40, 50, 60, 70, 80, 90 
20. HISTOLOGY: no, yes

## Objective
Our objective is to use Random forests to classifying whether the target patient as will die or will live based on otherindependent factors. We can then calculate essential metrics such as the confusion matrix and hence Sensitivity,specificity of the test.
Though there are many missing values, random forests performs better at handling missing values. We can also plot Variable importance to identify important factors affecting th life of the patient.
