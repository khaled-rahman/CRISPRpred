#' Illustration of Feature Validation
#'
#' This function takes featurelist, dataset and a significance value as input and produce a list of relevant features validated by anova test.
#'
#' @param featurelist a list of selected features
#' @param data provided as dataframe
#' @param significance a significance value for anova test
#' @return a list of selected and validated features
#' @export
#' @examples
#' featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' features = featurevalidation(featurelist, data, 0.01)
featurevalidation = function(featurelist, data, significance = 0.05){
  formuli = featureformula(featurelist)
  model = lm(as.formula(formuli), data)
  ap = anova(model)[5]
  p = ap$`Pr(>F)`
  f = row.names(ap)
  p = p[-length(p)]
  f = f[-length(f)]
  relevantfeatures = c()
  featuresignificance = c()
  for (i in 1:length(p)) {
    if (p[i] < significance) {
      relevantfeatures[length(relevantfeatures) + 1] = f[i]
      featuresignificance[length(featuresignificance) + 1] = p[i]
    }
  }
  validatedfeaturelist = data.frame(relevantfeatures,featuresignificance)
  return(validatedfeaturelist)
}