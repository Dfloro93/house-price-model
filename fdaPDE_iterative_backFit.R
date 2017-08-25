rm(list=ls())
library(fdaPDE)
library(sp)



path = "~/Desktop/RealEstatePublished";
covariates = c("effective_build_year", "net_area", "basement_finished", 
               "has_fireplace", "fully_complete", "lot_size", 
               "walkout_basement", "air_conditioning", 
               "site_coverage", "valuation_group_residential.south")

global_file = "global_file.Rdata"

# loading train data

load_train_data_fea = function(path){
  setwd(path)
  train_data = read.csv("data.subset.train.clustered.csv", header = TRUE)
  train_boundary = read.csv("data.subset.boundary.csv", header = FALSE)
  
  region_0_train_ind = (train_data[, "clust2"] == 0)
  region_1_train_ind = (train_data[, "clust2"] == 1)
  region_2_train_ind = (train_data[, "clust2"] == 2)
  
  region_1_train_boundary = read.csv("boundary21.csv", header = FALSE)
  region_2_train_boundary = read.csv("boundary22.csv", header = FALSE)
  
  train_coord = train_data[,c("lon","lat")]
  train_value = as.numeric(train_data[,"assessed_value"])
  
  region_1_train_coord = train_coord[region_1_train_ind,]
  region_1_train_value = train_value[region_1_train_ind]
  
  region_2_train_coord = train_coord[region_2_train_ind,]
  region_2_train_value = train_value[region_2_train_ind]
  
  train_cov = train_data[,covariates]
  train_cov = matrix(as.numeric(unlist(train_cov)),nrow=nrow(train_cov))
  colnames(train_cov) = covariates
  train_cov = as.data.frame(train_cov)
  region_1_train_cov = train_cov[region_1_train_ind,]
  region_2_train_cov = train_cov[region_2_train_ind,]
  
  output = list(  data = train_data,
                  boundary = train_boundary,
                  region_1_boundary = region_1_train_boundary,
                  region_2_boundary = region_2_train_boundary,
                  coord = train_coord,
                  region_1_coord = region_1_train_coord,
                  region_2_coord = region_2_train_coord,
                  value = train_value,
                  region_1_value = region_1_train_value,
                  region_2_value = region_2_train_value,
                  cov = train_cov,
                  region_1_cov = region_1_train_cov,
                  region_2_cov = region_2_train_cov,
                  ind = rep(T, length(train_value)),
                  region_1_ind = region_1_train_ind,
                  region_2_ind = region_2_train_ind
  )
  
  return(output)
  
}

load_train_data_geo = function(path){
  setwd(path)
  train_data = read.csv("data.subset.train.clustered.csv", header = TRUE)
  train_boundary = read.csv("data.subset.boundary.csv", header = FALSE)
  
  region_0_train_ind = (train_data[, "clust"] == 0)
  region_1_train_ind = (train_data[, "clust"] == 1)
  region_2_train_ind = (train_data[, "clust"] == 2)
  region_3_train_ind = (train_data[, "clust"] == 3)
  
  region_1_train_boundary = read.csv("boundary11.csv", header = FALSE)
  region_2_train_boundary = read.csv("boundary12.csv", header = FALSE)
  region_3_train_boundary = read.csv("boundary13.csv", header = FALSE)
  
  train_coord = train_data[,c("lon","lat")]
  train_value = as.numeric(train_data[,"assessed_value"])
  
  region_1_train_coord = train_coord[region_1_train_ind,]
  region_1_train_value = train_value[region_1_train_ind]
  
  region_2_train_coord = train_coord[region_2_train_ind,]
  region_2_train_value = train_value[region_2_train_ind]
  
  region_3_train_coord = train_coord[region_3_train_ind,]
  region_3_train_value = train_value[region_3_train_ind]
  
  train_cov = train_data[,covariates]
  train_cov = matrix(as.numeric(unlist(train_cov)),nrow=nrow(train_cov))
  colnames(train_cov) = covariates
  train_cov = as.data.frame(train_cov)
  region_1_train_cov = train_cov[region_1_train_ind,]
  region_2_train_cov = train_cov[region_2_train_ind,]
  region_3_train_cov = train_cov[region_3_train_ind,]
  
  output = list(  data = train_data,
                  boundary = train_boundary,
                  region_1_boundary = region_1_train_boundary,
                  region_2_boundary = region_2_train_boundary,
                  region_3_boundary = region_3_train_boundary,
                  coord = train_coord,
                  region_1_coord = region_1_train_coord,
                  region_2_coord = region_2_train_coord,
                  region_3_coord = region_3_train_coord,
                  value = train_value,
                  region_1_value = region_1_train_value,
                  region_2_value = region_2_train_value,
                  region_3_value = region_3_train_value,
                  cov = train_cov,
                  region_1_cov = region_1_train_cov,
                  region_2_cov = region_2_train_cov,
                  region_3_cov = region_3_train_cov,
                  ind = rep(T, length(train_value)),
                  region_1_ind = region_1_train_ind,
                  region_2_ind = region_2_train_ind,
                  region_3_ind = region_3_train_ind
  )
  
  return(output)
  
}

load_test_data_fea = function(path){
  setwd(path)
  test_data = read.csv("data.subset.test.clustered.csv", header = TRUE)

  region_0_test_ind = (test_data[, "clust2"] == 0)
  region_1_test_ind = (test_data[, "clust2"] == 1)
  region_2_test_ind = (test_data[, "clust2"] == 2)

  test_coord = test_data[,c("lon","lat")]
  test_value = as.numeric(test_data[,"assessed_value"])
  
  region_1_test_coord = test_coord[region_1_test_ind,]
  region_1_test_value = test_value[region_1_test_ind]
  
  region_2_test_coord = test_coord[region_2_test_ind,]
  region_2_test_value = test_value[region_2_test_ind]
  
  test_cov = test_data[,covariates]
  test_cov = matrix(as.numeric(unlist(test_cov)),nrow=nrow(test_cov))
  colnames(test_cov) = covariates
  test_cov = as.data.frame(test_cov)
  region_1_test_cov = test_cov[region_1_test_ind,]
  region_2_test_cov = test_cov[region_2_test_ind,]
  
  output = list(  data = test_data,
                  coord = test_coord,
                  region_1_coord = region_1_test_coord,
                  region_2_coord = region_2_test_coord,
                  value = test_value,
                  region_1_value = region_1_test_value,
                  region_2_value = region_2_test_value,
                  cov = test_cov,
                  region_1_cov = region_1_test_cov,
                  region_2_cov = region_2_test_cov,
                  ind = rep(T, length(test_value)),
                  region_1_ind = region_1_test_ind,
                  region_2_ind = region_2_test_ind
  )
  
  return(output)
  
}

load_test_data_geo = function(path){
  setwd(path)
  test_data = read.csv("data.subset.test.clustered.csv", header = TRUE)

  region_0_test_ind = (test_data[, "clust"] == 0)
  region_1_test_ind = (test_data[, "clust"] == 1)
  region_2_test_ind = (test_data[, "clust"] == 2)
  region_3_test_ind = (test_data[, "clust"] == 3)

  test_coord = test_data[,c("lon","lat")]
  test_value = as.numeric(test_data[,"assessed_value"])
  
  region_1_test_coord = test_coord[region_1_test_ind,]
  region_1_test_value = test_value[region_1_test_ind]
  
  region_2_test_coord = test_coord[region_2_test_ind,]
  region_2_test_value = test_value[region_2_test_ind]
  
  region_3_test_coord = test_coord[region_3_test_ind,]
  region_3_test_value = test_value[region_3_test_ind]
  
  test_cov = test_data[,covariates]
  test_cov = matrix(as.numeric(unlist(test_cov)),nrow=nrow(test_cov))
  colnames(test_cov) = covariates
  test_cov = as.data.frame(test_cov)
  region_1_test_cov = test_cov[region_1_test_ind,]
  region_2_test_cov = test_cov[region_2_test_ind,]
  region_3_test_cov = test_cov[region_3_test_ind,]
  
  output = list(  data = test_data,
                  coord = test_coord,
                  region_1_coord = region_1_test_coord,
                  region_2_coord = region_2_test_coord,
                  region_3_coord = region_3_test_coord,
                  value = test_value,
                  region_1_value = region_1_test_value,
                  region_2_value = region_2_test_value,
                  region_3_value = region_3_test_value,
                  cov = test_cov,
                  region_1_cov = region_1_test_cov,
                  region_2_cov = region_2_test_cov,
                  region_3_cov = region_3_test_cov,
                  ind = rep(T, length(test_value)),
                  region_1_ind = region_1_test_ind,
                  region_2_ind = region_2_test_ind,
                  region_3_ind = region_3_test_ind
  )
  
  return(output)
  
}


iterate_surface = function(train, test, train_index, test_index, boundary, max_iter, msg, log = F, lambda){
  load(global_file)
  
  train_value = global$train_residuals[train_index]
  test_value = global$test_residuals[test_index]

  
  MSE = rep(0, max_iter)
  log_MSE = rep(0, max_iter)
  rel_err = rep(0, max_iter)
  
  overall_MSE = rep(0, max_iter)
  overall_log_MSE = rep(0, max_iter)
  overall_rel_err = rep(0, max_iter)
  
  # cluster residuals
  MSE_1 = rep(0, max_iter)
  MSE_2 = rep(0, max_iter)
  log_MSE_1 = rep(0, max_iter)
  log_MSE_2 = rep(0, max_iter)
  
  
  print(msg)
  
  estimated_train_value_ols = rep(0, length(train_value))
  
  for(i in 1:max_iter){
    cat("\n Iteration ", i, " out of ", max_iter, "\n")
    # train
    # f_0
    order = 1
    mesh = create.MESH.2D(nodes = train$coord[train_index,],
                          segments = boundary, order = order)
    #plot(mesh)
    FEMbasis = create.FEM.basis(mesh)
    EstimatedSurface = smooth.FEM.basis(observations = train_value - estimated_train_value_ols,
                                        FEMbasis = FEMbasis, lambda = lambda, CPP_CODE = T)
    estimated_train_value_f = eval.FEM(EstimatedSurface$fit.FEM,
                                       train$coord[train_index,], CPP_CODE = F)
    # OLS
    train_residuals_f = train_value - estimated_train_value_f
    fit_ols = lm(train_residuals_f ~ ., data = train$cov[train_index,])
    estimated_train_value_ols = fit_ols$fitted.values
    
    # collecting residuals
    estimated_train_value = estimated_train_value_f + estimated_train_value_ols
    train_residuals = train_value - estimated_train_value
    
    # test
    
    estimated_test_value_f = eval.FEM(EstimatedSurface$fit.FEM,
                                      test$coord[test_index,], CPP_CODE = F)
    # test OLS and MSE
    estimated_test_value_ols = predict(fit_ols, newdata = test$cov[test_index,])
    estimated_test_value = estimated_test_value_f + estimated_test_value_ols
    test_residuals = test_value - estimated_test_value
    if(log){
      # convert before passing
      exp_train_residuals = exp(train_value) - exp(estimated_train_value)
      exp_test_residuals = exp(test_value) - exp(estimated_test_value)
      # update global residuals
      global$train_residuals[train_index] = exp_train_residuals
      global$test_residuals[test_index] = exp_test_residuals
      global$train_log_residuals[train_index] = train_residuals
      global$test_log_residuals[test_index] = test_residuals
      
      log_MSE[i] = mean(global$test_log_residuals[test_index]^2)
      MSE[i] = mean(global$test_residuals[test_index]^2)
      rel_err[i] = mean(abs(global$test_residuals[test_index])/test$value[test_index])
      
      overall_log_MSE[i] = mean(global$test_log_residuals^2)
      overall_MSE[i] = mean(global$test_residuals^2)
      overall_rel_err[i] = mean(abs(global$test_residuals)/test$value)
      #collecting cluster residuals
      MSE_1[i] = mean((global$test_residuals[test$region_1_ind])^2) 
      MSE_2[i] = mean((global$test_residuals[test$region_2_ind])^2) 
      log_MSE_1[i] = mean((global$test_log_residuals[test$region_1_ind])^2) 
      log_MSE_2[i] = mean((global$test_log_residuals[test$region_2_ind])^2) 
      
    } else{
      global$train_residuals[train_index] = train_residuals
      global$test_residuals[test_index] = test_residuals
      
      MSE[i] = mean((global$test_residuals[test_index])^2)
      rel_err[i] = mean(abs(global$test_residuals[test_index])/test$value[test_index])
      
      overall_MSE[i] = mean((global$test_residuals)^2)
      overall_rel_err[i] = mean(abs(global$test_residuals)/test$value)
    }
    cat("\n MSE 1e+10", MSE[i]/1e+10, " || overall MSE 1e+10", overall_MSE[i]/1e+10,"\n")
    #cat("\n log_MSE ", log_MSE[i], " || overall log_MSE", overall_log_MSE[i],"\n")
    #cat("\n rel_err ", rel_err[i], " || overall rel_err", overall_rel_err[i],"\n")
  }
  
  save(global, file = global_file)
  rm(global)
  
  if(log){
    output = list(MSE = MSE, 
                  log_MSE = log_MSE,
                  rel_err = rel_err,
                  overall_MSE = overall_MSE, 
                  overall_log_MSE = overall_log_MSE,
                  ovrall_rel_err = overall_rel_err,
                  MSE_1 = MSE_1,
                  MSE_2 = MSE_2,
                  log_MSE_1 = log_MSE_1,
                  log_MSE_2 = log_MSE_2
    )
  } else{
    output = list(MSE = MSE, 
                  rel_err = rel_err,
                  overall_MSE = overall_MSE, 
                  ovrall_rel_err = overall_rel_err
    )
  }
  return(output)
}

#loading all data
train_geo = load_train_data_geo(path)
train_fea = load_train_data_fea(path)
test_geo = load_test_data_geo(path)
test_fea = load_test_data_fea(path)

setwd(path)

global = list(
  train_residuals = train_fea$value,
  test_residuals = test_fea$value
)
save(global, file = global_file)
rm(global)

#10^-8, 10^-7, 10^-6 give tiny jumps (still visible)
lambda = 10^-7
log = F
max_iter = 30
train_index = train_fea$ind
test_index = test_fea$ind
boundary = train_fea$boundary
msg = 'Global surface'
global_output = iterate_surface(train_fea, test_fea, train_index, test_index, boundary, max_iter, msg, log = log, lambda)

#################### geo ####################
train_index = train_geo$region_1_ind
test_index = test_geo$region_1_ind
boundary = train_geo$region_1_boundary
msg = 'geo partition, element: 1'
region_geo_1_output = iterate_surface(train_geo, test_geo, train_index, test_index, boundary, max_iter, msg, log = log, lambda)

train_index = train_geo$region_2_ind
test_index = test_geo$region_2_ind
boundary = train_geo$region_2_boundary
msg = 'geo partition, element: 2'
region_geo_2_output = iterate_surface(train_geo, test_geo, train_index, test_index, boundary, max_iter, msg, log = log, lambda)

train_index = train_geo$region_3_ind
test_index = test_geo$region_3_ind
boundary = train_geo$region_3_boundary
msg = 'geo partition, element: 3'
region_geo_3_output = iterate_surface(train_geo, test_geo, train_index, test_index, boundary, max_iter, msg, log = log, lambda)

#################### fea ####################
train_index = train_fea$region_1_ind
test_index = test_fea$region_1_ind
boundary = train_fea$region_1_boundary
msg = 'fea partition, element: 1'
region_fea_1_output = iterate_surface(train_fea, test_fea, train_index, test_index, boundary, max_iter, msg, log = log, lambda)

train_index = train_fea$region_2_ind
test_index = test_fea$region_2_ind
boundary = train_fea$region_2_boundary
msg = 'fea partition, element: 2'
region_fea_2_output = iterate_surface(train_fea, test_fea, train_index, test_index, boundary, max_iter, msg, log = log, lambda)



MSE = c(global_output$overall_MSE, region_geo_1_output$overall_MSE, region_geo_2_output$overall_MSE, region_geo_3_output$overall_MSE,
        region_fea_1_output$overall_MSE, region_fea_2_output$overall_MSE)
plot(MSE)
