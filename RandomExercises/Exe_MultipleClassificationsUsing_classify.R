# This did not work because the package either was out of date or 
# soemthing!!! Nov 2013


# Learning classify package which uses jags to compare predictive models
## read positive/negative sequence from files.
tmpfile1 = file.path(path.package("BioSeqClass"), "example", "acetylation_K.pos40.pep")
tmpfile2 = file.path(path.package("BioSeqClass"), "example", "acetylation_K.neg40.pep")
posSeq = as.matrix(read.csv(tmpfile1,header=FALSE,sep="\t",row.names=1))[,1]
negSeq = as.matrix(read.csv(tmpfile2,header=FALSE,sep="\t",row.names=1))[,1]
seq=c(posSeq,negSeq)
classLable=c(rep("+1",length(posSeq)),rep("-1",length(negSeq)) ) 
data = data.frame(featureBinary(seq),classLable)

## Use LibSVM and 5-cross-validation to classify.
LIBSVM_CV5 = classify(data,classifyMethod="libsvm",cv=5,
                      svm.kernel="linear",svm.scale=FALSE)
## Features selection is done by envoking "CfsSubsetEval" method in WEKA.               
FS_LIBSVM_CV5 = classify(data,classifyMethod="libsvm",cv=5,evaluator="CfsSubsetEval",
                         search="BestFirst",svm.kernel="linear",svm.scale=FALSE)    

if(interactive()){
  
  KNN_CV5 = classify(data,classifyMethod="knn",cv=5,knn.k=1)  
  
  RF_CV5 = classify(data,classifyMethod="randomForest",cv=5)
  
  TREE_CV5 = classify(data,classifyMethod="tree",cv=5)
  
  NNET_CV5 = classify(data,classifyMethod="nnet",cv=5)
  
  RPART_CV5 = classify(data,classifyMethod="rpart",cv=5,evaluator="")
  
  CTREE_CV5 = classify(data,classifyMethod="ctree",cv=5,evaluator="")  
  
  BAG_CV5 = classify(data,classifyMethod="bagging",cv=5,evaluator="")  
  
}
