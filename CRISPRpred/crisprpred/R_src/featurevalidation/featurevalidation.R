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