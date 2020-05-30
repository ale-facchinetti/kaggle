# backup
levels(dataset_train$Alley) = c(levels(dataset_train$Alley), 'No alley')
dataset_train$Alley[dataset_train$Alley == 'NA'] = 'No alley'

levels(dataset_train$BsmtQual) = c(levels(dataset_train$BsmtQual), 'No basement')
dataset_train$BsmtQual[dataset_train$BsmtQual == 'NA'] = 'No basement'

levels(dataset_train$BsmtCond) = c(levels(dataset_train$BsmtCond), 'No basement')
dataset_train$BsmtCond[dataset_train$BsmtCond == 'NA'] = 'No basement'

levels(dataset_train$BsmtExposure) = c(levels(dataset_train$BsmtExposure), 'No basement')
dataset_train$BsmtExposure[dataset_train$BsmtExposure == 'NA'] = 'No basement'

levels(dataset_train$BsmtFinType1) = c(levels(dataset_train$BsmtFinType1), 'No basement')
dataset_train$BsmtFinType1[dataset_train$BsmtFinType1 == 'NA'] = 'No basement'

levels(dataset_train$BsmtFinType2) = c(levels(dataset_train$BsmtFinType2), 'No basement')
dataset_train$BsmtFinType2[dataset_train$BsmtFinType2 == 'NA'] = 'No basement'

levels(dataset_train$GarageFinish) = c(levels(dataset_train$GarageFinish), 'No garage')
dataset_train$GarageFinish[dataset_train$GarageFinish == 'NA'] = 'No garage'

levels(dataset_train$GarageQual) = c(levels(dataset_train$GarageQual), 'No garage')
dataset_train$GarageQual[dataset_train$GarageQual == 'NA'] = 'No garage'

levels(dataset_train$GarageCond) = c(levels(dataset_train$GarageCond), 'No garage')
dataset_train$GarageCond[dataset_train$GarageCond == 'NA'] = 'No garage'

levels(dataset_train$PoolQC) = c(levels(dataset_train$PoolQC), 'No pool')
dataset_train$PoolQC[dataset_train$PoolQC == 'NA'] = 'No pool'

levels(dataset_train$Fence) = c(levels(dataset_train$Fence), 'No fence')
dataset_train$Fence[dataset_train$Fence == 'NA'] = 'No fence'

levels(dataset_train$MiscFeature) = c(levels(dataset_train$MiscFeature), 'None')
dataset_train$MiscFeature[dataset_train$MiscFeature == 'NA'] = 'None'

str(dataset_train$Alley)
