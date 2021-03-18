# chouffe_label_checker

### To find labelling errors made by humans in the Chouffe validation tool
### Uses a combination of the Stupid Matcher and a series of SVMs

### Currently only a POC in R (will be moved to Python eventually)

Steps:
1. Take all mds geometry used in validation and join to all surrounding atlas geometry in 15m
2. Run a Stupid Matcher on these geometry pairs
3. We get 4 GIS variables from Stupid Matcher: 
    - line azimuth difference, 
    - length of shared line section in 10m buffer,
    - proportion of shorter line contained in 50m buffer of longer line
    - minimum separation distance of the two lines
 4. Fit SVM on all data
 5. Split the full dataset into two new sets: those that are support vectors and those that are not
 6. Fit a new SVM on only the non-support vectors (a proxy for a perfect training set)
 7. Use this new model to predict the support vector set
 8. Misclassifications where humans and SVM model can now be ranked according to probability
 
