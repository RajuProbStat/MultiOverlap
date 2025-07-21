####function to evaluate the point and confidence interval estimates of the
##containment and overlap indices for multivariate normally distributed populations
#install.packages("MASS")

#' To compute the point and interval estimates of the containment and overlap indices J1, J2 and J for multivariate normal distributions
#'
#' @param data1 data1 is the data frame for first population F having n1 rows and d columns, where n1 is the sample size and d is the dimension
#' @param data2 data2 is the data frame for second population G having n2 rows and d columns, where n2 is the sample size and d is the dimension
#'
#' @return numeric values consisting of point and interval estimates of the indices J1, J2 and J
#' @export
#'
#' @examples
#' data1<-rbind(
#'  c(193.12, 36.36, 19.45, 83.00, 0.74, 32.03),
#'  c(296.64, 27.87, 18.00, 99.27, 1.07, 30.70),
#'  c(181.63, 21.96, 16.65, 85.63, 0.61, 6.800),
#'  c(191.05, 38.30, 14.44, 57.86, 0.80, 6.880),
#'  c(252.83, 24.37, 24.11, 111.7, 0.98, 25.23),
#'  c(165.77, 23.60, 12.00, 65.64, 1.14, 17.00),
#'  c(174.36, 24.53, 14.36, 44.15, 1.70, 16.26),
#'  c(264.98, 24.27, 13.12, 59.00, 2.10, 11.14),
#'  c(210.78, 20.71, 18.71, 45.73, 1.26, 21.02),
#'  c(214.26, 19.18, 22.46, 71.98, 1.13, 26.68),
#'  c(289.60, 15.72, 18.98, 76.99, 0.58, 17.96),
#'  c(292.75, 15.69, 26.60, 63.98, 0.65, 27.34),
#'  c(170.60, 27.51, 31.57, 57.80, 0.23, 6.420),
#'  c(204.45, 36.30, 17.25, 49.44, 0.58, 17.39),
#'  c(140.01, 33.71, 15.06, 50.34, 0.60, 22.14),
#'  c(109.69, 35.38, 22.07, 53.59, 0.60, 26.11),
#'  c(287.18, 52.38, 25.00, 53.99, 0.60, 21.72),
#'  c(254.05, 40.42, 28.93, 79.80, 0.60, 16.07),
#'  c(285.13, 49.44, 24.90, 74.12, 0.48, 16.85),
#'  c(365.83, 38.21, 12.58, 52.48, 0.38, 15.27),
#'  c(71.160, 41.76, 14.43, 30.61, 0.22, 10.39),
#'  c(132.23, 43.39, 15.33, 13.06, 0.34, 10.36),
#'  c(150.67, 65.49, 20.99, 91.54, 1.43, 10.88),
#'  c(138.94, 49.18, 23.96, 49.64, 1.55, 15.49),
#'  c(171.60, 65.37, 14.33, 55.94, 1.06, 11.85),
#'  c(72.160, 39.98, 19.30, 26.67, 1.42, 14.04),
#'  c(185.25, 50.79, 23.29, 43.67, 0.67, 14.05),
#'  c(117.02, 31.58, 19.03, 33.47, 1.58, 15.94),
#'  c(137.29, 36.92, 21.72, 45.19, 1.61, 14.96),
#'  c(170.66, 43.30, 19.04, 62.37, 1.30, 17.30))
#' data2<-rbind(
#' c(281.93, 32.89, 19.40, 91.080, 0.43, 41.83),
#' c(221.60, 43.08, 17.72, 68.580, 0.28, 23.48),
#' c(247.94, 44.32, 20.08, 67.790, 0.28, 16.39),
#' c(340.41, 64.82, 27.63, 99.820, 0.45, 32.41),
#' c(354.67, 40.91, 13.48, 93.500, 0.73, 27.85),
#' c(155.16, 31.02, 12.98, 44.010, 0.33, 22.92),
#' c(202.24, 47.42, 16.95, 93.850, 0.55, 24.33),
#' c(253.36, 36.01, 18.82, 89.560, 0.32, 29.78),
#' c(180.59, 37.63, 23.40, 101.93, 0.27, 53.75),
#' c(336.75, 55.31, 17.05, 114.65, 0.76, 25.80),
#' c(217.31, 51.38, 19.38, 45.200, 0.71, 57.62),
#' c(286.55, 43.20, 20.62, 75.890, 0.52, 45.91),
#' c(181.39, 45.99, 27.05, 36.700, 0.56, 54.53),
#' c(213.13, 51.90, 22.50, 79.820, 0.64, 35.90),
#' c(217.98, 38.44, 16.75, 51.190, 0.83, 43.78),
#' c(267.99, 41.07, 16.09, 70.080, 0.72, 52.86),
#' c(162.81, 24.04, 14.83, 27.540, 0.48, 64.86),
#' c(168.70, 24.25, 17.84, 19.540, 0.21, 74.29),
#' c(198.67, 59.48, 21.63, 52.120, 0.86, 38.98),
#' c(318.52, 53.84, 28.05, 125.03, 1.12, 49.23),
#' c(213.09, 50.01, 6.760, 60.130, 0.36, 34.38),
#' c(222.60, 51.18, 11.30, 67.270, 0.63, 37.74),
#' c(258.11, 55.51, 12.33, 83.740, 0.48, 44.42),
#' c(241.36, 48.70, 22.59, 67.030, 0.92, 40.42),
#' c(245.27, 40.36, 20.29, 63.850, 0.91, 31.73),
#' c(154.72, 29.56, 20.69, 36.770, 0.63, 40.70),
#' c(160.04, 36.11, 29.72, 39.120, 0.72, 39.44),
#' c(149.48, 31.54, 31.05, 41.410, 0.42, 37.09),
#' c(162.90, 35.09, 29.09, 57.940, 0.59, 39.40),
#' c(133.15, 28.44, 19.34, 35.380, 0.58, 36.15),
#' c(184.35, 34.43, 16.77, 54.080, 0.59, 26.05),
#' c(190.54, 43.80, 28.10, 54.070, 0.93, 47.74),
#' c(227.74, 38.47, 27.59, 72.390, 0.91, 45.12),
#' c(200.15, 43.61, 19.62, 37.380, 0.66, 47.76),
#' c(101.13, 29.36, 20.97, 42.140, 0.58, 32.22),
#' c(163.79, 29.58, 26.28, 67.460, 1.07, 61.70),
#' c(69.940, 16.52, 21.13, 20.120, 0.47, 26.17),
#' c(57.040, 11.75, 20.76, 23.690, 0.46, 29.58),
#' c(149.93, 34.72, 13.35, 46.790, 0.67, 48.68),
#' c(51.390, 14.46, 15.85, 15.940, 0.44, 38.08),
#' c(187.78, 20.68, 20.33, 61.280, 0.47, 44.95),
#' c(147.60, 24.03, 17.97, 44.250, 0.65, 47.29),
#' c(176.26, 34.29, 11.53, 58.240, 0.58, 58.57),
#' c(122.64, 32.83, 21.84, 37.770, 0.39, 61.83),
#' c(42.620, 19.14, 12.66, 15.660, 0.45, 32.32))
#' MVN_Overlap(data1,data2)
MVN_Overlap<-function(data1,data2){
  library(MASS)
  data1<-data1                        #data set for the first population
  data2<-data2                        #data set for the second population
  n1<-length(data1[,1])               #sample size for the first population
  n2<-length(data2[,1])               #sample size for the second population
  data1_mean_est<-apply(data1,2,mean) #mean vector for the first population
  data1_cov_est<-cov(data1)           #covariance matrix for the first population
  data2_mean_est<-apply(data2,2,mean) #mean vector for the second population
  data2_cov_est<-cov(data2)           #covariance matrix for the second population

  ##for the estimated values of the containment and overlap indices
  dep1<-rep(NA,n1) #for depth values of the first population
  dep2<-rep(NA,n2) #for depth values of the second population
  fre1<-rep(0,n2)
  fre2<-rep(0,n1)
  for (i in 1:n1)
  {
    dep1[i]<-1/(1+t((data1[i,]-data1_mean_est))%*%solve(data1_cov_est)%*%(data1[i,]-data1_mean_est))
  }
  for (j in 1:n2)
  {
    dep2[j]<-1/(1+t((data2[j,]-data2_mean_est))%*%solve(data2_cov_est)%*%(data2[j,]-data2_mean_est))
  }
  dep1_new<-sort(dep1, decreasing=TRUE)
  dep2_new<-sort(dep2,decreasing=TRUE)
  for (j in 1:n2)
  {
    for (i in 1:n1)
    {
      if ((1/(1+t((data1[i,]-data2_mean_est))%*%solve(data2_cov_est)%*%(data1[i,]-data2_mean_est)))>=dep2_new[j])
      {
        fre1[j]<-fre1[j]+1
      }
      else
      {
        fre1[j]<-fre1[j]
      }
    }
  }
  J1_hat<-sum(fre1)/(n1*n2) #estimated value of J1
  for (i in 1:n1)
  {
    for (j in 1:n2)
    {
      if ((1/(1+t((data2[j,]-data1_mean_est))%*%solve(data1_cov_est)%*%(data2[j,]-data1_mean_est)))>=dep1_new[i])
      {
        fre2[i]<-fre2[i]+1
      }
      else
      {
        fre2[i]<-fre2[i]
      }
    }
  }
  J2_hat<-sum(fre2)/(n1*n2) #estimated value of J2
  J_hat<-4*J1_hat*J2_hat    #estimated value of J

  ##for the bootstrap confidence intervals of the containment and overlap indices
  alpha_level<-0.05       #(1-alpha_level) is the confidence coefficient
  B<-2000                 #number of bootstrap replicates
  J1_hat_boot<-rep(NA,B)
  J2_hat_boot<-rep(NA,B)
  J_hat_boot<-rep(NA,B)
  for (b in 1:B)
  {
    set.seed(17*b)
    data1_boot<-mvrnorm(n1,data1_mean_est,data1_cov_est) #bootstrap sample for first population
    data2_boot<-mvrnorm(n2,data2_mean_est,data2_cov_est) #bootstrap sample for second population
    ##estimated values of the containment and overlap indices based on bootstrap samples
    data1_mean_est_boot<-apply(data1_boot,2,mean)
    data1_cov_est_boot<-cov(data1_boot)
    data2_mean_est_boot<-apply(data2_boot,2,mean)
    data2_cov_est_boot<-cov(data2_boot)
    dep1_boot<-rep(NA,n1)
    dep2_boot<-rep(NA,n2)
    fre1_boot<-rep(0,n2)
    fre2_boot<-rep(0,n1)
    for(i in 1:n1)
    {
      dep1_boot[i]<-1/(1+t((data1_boot[i,]-data1_mean_est_boot))%*%solve(data1_cov_est_boot)%*%(data1_boot[i,]-data1_mean_est_boot))
    }
    for(j in 1:n2)
    {
      dep2_boot[j]<-1/(1+t((data2_boot[j,]-data2_mean_est_boot))%*%solve(data2_cov_est_boot)%*%(data2_boot[j,]-data2_mean_est_boot))
    }
    dep1_sort_boot<-sort(dep1_boot, decreasing=TRUE) #ordered depth values of first population
    dep2_sort_boot<-sort(dep2_boot, decreasing=TRUE) #ordered depth values of second population
    for(j in 1:(n2))
    {
      for(i in 1:n1)
      {
        if ((1/(1+t((data1_boot[i,]-data2_mean_est_boot))%*%solve(data2_cov_est_boot)%*%(data1_boot[i,]-data2_mean_est_boot)))>=dep2_sort_boot[j])
        {
          fre1_boot[j]<-fre1_boot[j]+1
        }
        else
        {
          fre1_boot[j]<-fre1_boot[j]
        }
      }
    }
    J1_hat_boot[b]<-sum(fre1_boot)/(n1*n2)          #estimated value of J1 based on bootstrap samples
    for(i in 1:(n1))
    {
      for(j in 1:n2)
      {
        if ((1/(1+t((data2_boot[j,]-data1_mean_est_boot))%*%solve(data1_cov_est_boot)%*%(data2_boot[j,]-data1_mean_est_boot)))>=dep1_sort_boot[i])
        {
          fre2_boot[i]<-fre2_boot[i]+1
        }
        else
        {
          fre2_boot[i]<-fre2_boot[i]
        }
      }
    }
    J2_hat_boot[b]<-sum(fre2_boot)/(n1*n2)         #estimated value of J2 based on bootstrap samples
    J_hat_boot[b]<-4*J1_hat_boot[b]*J2_hat_boot[b] #estimated value of J based on bootstrap samples
  }
  q1_J1<-quantile((J1_hat_boot-J1_hat),(alpha_level/2),names=FALSE)
  q2_J1<-quantile((J1_hat_boot-J1_hat),(1-(alpha_level/2)),names=FALSE)
  q1_J2<-quantile((J2_hat_boot-J2_hat),(alpha_level/2),names=FALSE)
  q2_J2<-quantile((J2_hat_boot-J2_hat),(1-(alpha_level/2)),names=FALSE)
  q1_J<-quantile((J_hat_boot-J_hat),(alpha_level/2),names=FALSE)
  q2_J<-quantile((J_hat_boot-J_hat),(1-(alpha_level/2)),names=FALSE)

  ##bootstrap confidence interval for J1
  lower_J1<-J1_hat+q1_J1          #lower limit of the bootstrap CI of J1
  upper_J1<-J1_hat+q2_J1          #upper limit of the bootstrap CI of J1

  ##bootstrap confidence interval for J2
  lower_J2<-J2_hat+q1_J2          #lower limit of the bootstrap CI of J2
  upper_J2<-J2_hat+q2_J2          #upper limit of the bootstrap CI of J2

  ##bootstrap confidence interval for J
  lower_J<-J_hat+q1_J             #lower limit of the bootstrap CI of J
  upper_J<-J_hat+q2_J             #upper limit of the bootstrap CI of J

  ##result
  cat("*********************************************\n")
  cat("Estimated value of J1, J2 and J are:", J1_hat, J2_hat, J_hat)
  cat("\n Lower and upper limits of the 95% bootstrap confidence interval of J1 are:", lower_J1, upper_J1)
  cat("\n Lower and upper limits of the 95% bootstrap confidence interval of J2 are:", lower_J2, upper_J2)
  cat("\n Lower and upper limits of the 95% bootstrap confidence interval of J are:", lower_J, upper_J)
  cat("\n*********************************************")
}



################################################################################
####function to evaluate the point and confidence interval estimates of the
##containment and overlap indices for univariate normally distributed populations

#' To compute the point and interval estimates of the containment and overlap indices J1, J2 and J for univariate normal distributions
#'
#' @param data1 data1 is the data set for the univariate normally distributed population F
#' @param data2 data2 is the data set for the univariate normally distributed population G
#'
#' @return numeric values consisting of point and interval estimates of the indices J1, J2 and J for univariate normal distributions
#' @export
#'
#' @examples
#'data1<-rbind(
#'  c(193.12, 36.36, 19.45, 83.00, 0.74, 32.03),
#'  c(296.64, 27.87, 18.00, 99.27, 1.07, 30.70),
#'  c(181.63, 21.96, 16.65, 85.63, 0.61, 6.800),
#'  c(191.05, 38.30, 14.44, 57.86, 0.80, 6.880),
#'  c(252.83, 24.37, 24.11, 111.7, 0.98, 25.23),
#'  c(165.77, 23.60, 12.00, 65.64, 1.14, 17.00),
#'  c(174.36, 24.53, 14.36, 44.15, 1.70, 16.26),
#'  c(264.98, 24.27, 13.12, 59.00, 2.10, 11.14),
#'  c(210.78, 20.71, 18.71, 45.73, 1.26, 21.02),
#'  c(214.26, 19.18, 22.46, 71.98, 1.13, 26.68),
#'  c(289.60, 15.72, 18.98, 76.99, 0.58, 17.96),
#'  c(292.75, 15.69, 26.60, 63.98, 0.65, 27.34),
#'  c(170.60, 27.51, 31.57, 57.80, 0.23, 6.420),
#'  c(204.45, 36.30, 17.25, 49.44, 0.58, 17.39),
#'  c(140.01, 33.71, 15.06, 50.34, 0.60, 22.14),
#'  c(109.69, 35.38, 22.07, 53.59, 0.60, 26.11),
#'  c(287.18, 52.38, 25.00, 53.99, 0.60, 21.72),
#'  c(254.05, 40.42, 28.93, 79.80, 0.60, 16.07),
#'  c(285.13, 49.44, 24.90, 74.12, 0.48, 16.85),
#'  c(365.83, 38.21, 12.58, 52.48, 0.38, 15.27),
#'  c(71.160, 41.76, 14.43, 30.61, 0.22, 10.39),
#'  c(132.23, 43.39, 15.33, 13.06, 0.34, 10.36),
#'  c(150.67, 65.49, 20.99, 91.54, 1.43, 10.88),
#'  c(138.94, 49.18, 23.96, 49.64, 1.55, 15.49),
#'  c(171.60, 65.37, 14.33, 55.94, 1.06, 11.85),
#'  c(72.160, 39.98, 19.30, 26.67, 1.42, 14.04),
#'  c(185.25, 50.79, 23.29, 43.67, 0.67, 14.05),
#'  c(117.02, 31.58, 19.03, 33.47, 1.58, 15.94),
#'  c(137.29, 36.92, 21.72, 45.19, 1.61, 14.96),
#'  c(170.66, 43.30, 19.04, 62.37, 1.30, 17.30))
#'  data2<-rbind(
#' c(281.93, 32.89, 19.40, 91.080,	0.43,	41.83),
#' c(221.60,	43.08, 17.72, 68.580,	0.28,	23.48),
#' c(247.94,	44.32, 20.08, 67.790,	0.28,	16.39),
#' c(340.41,	64.82, 27.63, 99.820,	0.45,	32.41),
#' c(354.67,	40.91, 13.48, 93.500,	0.73,	27.85),
#' c(155.16,	31.02, 12.98, 44.010,	0.33,	22.92),
#' c(202.24,	47.42, 16.95, 93.850,	0.55,	24.33),
#' c(253.36,	36.01, 18.82, 89.560,	0.32,	29.78),
#' c(180.59,	37.63, 23.40, 101.93,	0.27,	53.75),
#' c(336.75,	55.31, 17.05, 114.65,	0.76, 25.80),
#' c(217.31,	51.38, 19.38, 45.200,	0.71,	57.62),
#' c(286.55,	43.20, 20.62, 75.890,	0.52,	45.91),
#' c(181.39,	45.99, 27.05, 36.700,	0.56,	54.53),
#' c(213.13,	51.90, 22.50, 79.820,	0.64,	35.90),
#' c(217.98,	38.44, 16.75, 51.190,	0.83,	43.78),
#' c(267.99,	41.07, 16.09, 70.080,	0.72,	52.86),
#' c(162.81,	24.04, 14.83, 27.540,	0.48,	64.86),
#' c(168.70,	24.25, 17.84, 19.540,	0.21,	74.29),
#' c(198.67,	59.48, 21.63, 52.120,	0.86,	38.98),
#' c(318.52,	53.84, 28.05, 125.03,	1.12,	49.23),
#' c(213.09,	50.01, 6.760, 60.130,	0.36,	34.38),
#' c(222.60,	51.18, 11.30, 67.270,	0.63,	37.74),
#' c(258.11,	55.51, 12.33, 83.740,	0.48,	44.42),
#' c(241.36,	48.70, 22.59, 67.030,	0.92,	40.42),
#' c(245.27,	40.36, 20.29, 63.850,	0.91,	31.73),
#' c(154.72,	29.56, 20.69, 36.770,	0.63,	40.70),
#' c(160.04,	36.11, 29.72, 39.120,	0.72,	39.44),
#' c(149.48,	31.54, 31.05, 41.410,	0.42,	37.09),
#' c(162.90,	35.09, 29.09, 57.940,	0.59,	39.40),
#' c(133.15,	28.44, 19.34, 35.380,	0.58, 36.15),
#' c(184.35,	34.43, 16.77, 54.080,	0.59,	26.05),
#' c(190.54,	43.80, 28.10, 54.070,	0.93,	47.74),
#' c(227.74,	38.47, 27.59, 72.390,	0.91,	45.12),
#' c(200.15,	43.61, 19.62, 37.380,	0.66,	47.76),
#' c(101.13, 29.36, 20.97, 42.140,	0.58,	32.22),
#' c(163.79,	29.58, 26.28, 67.460,	1.07,	61.70),
#' c(69.940,	16.52, 21.13, 20.120,	0.47,	26.17),
#' c(57.040,	11.75, 20.76, 23.690,	0.46,	29.58),
#' c(149.93,	34.72, 13.35, 46.790,	0.67,	48.68),
#' c(51.390,	14.46, 15.85, 15.940,	0.44,	38.08),
#' c(187.78,	20.68, 20.33, 61.280,	0.47,	44.95),
#' c(147.60,	24.03, 17.97, 44.250,	0.65,	47.29),
#' c(176.26,	34.29, 11.53, 58.240,	0.58,	58.57),
#' c(122.64,	32.83, 21.84, 37.770,	0.39,	61.83),
#' c(42.620,	19.14, 12.66, 15.660,	0.45,	32.32))
#' UVN_Overlap(data1[,6:6],data2[,6:6])
UVN_Overlap<-function(data1,data2)
{
  library(MASS)
  data1<-data1                #data set for the first population
  data2<-data2                #data set for the second population
  n1<-length(data1)           #sample size of the first population
  n2<-length(data2)           #sample size of the second population
  data1_mean_est<-mean(data1) #sample mean of the first population
  data1_cov_est<-var(data1)   #Sample variance of the first population
  data2_mean_est<-mean(data2) #sample mean of the second population
  data2_cov_est<-var(data2)   #sample variance of the second population
  dep1<-rep(NA,n1) #for depth values of the first population
  dep2<-rep(NA,n2) #for depth values of the second population
  fre1<-rep(0,n2)
  fre2<-rep(0,n1)
  for (i in 1:n1)
  {
    dep1[i]<-1/(1+t((data1[i]-data1_mean_est))%*%solve(data1_cov_est)%*%(data1[i]-data1_mean_est))
  }
  for (j in 1:n2)
  {
    dep2[j]<-1/(1+t((data2[j]-data2_mean_est))%*%solve(data2_cov_est)%*%(data2[j]-data2_mean_est))
  }
  dep1_new<-sort(dep1, decreasing=TRUE) #ordered depth values of first population
  dep2_new<-sort(dep2,decreasing=TRUE)  #ordered depth values of second population
  for (j in 1:n2)
  {
    for (i in 1:n1)
    {
      if ((1/(1+t((data1[i]-data2_mean_est))%*%solve(data2_cov_est)%*%(data1[i]-data2_mean_est)))>=dep2_new[j])
      {
        fre1[j]<-fre1[j]+1
      }
      else
      {
        fre1[j]<-fre1[j]
      }
    }
  }
  J1_hat<-sum(fre1)/(n1*n2) #estimated value of the containment index J1
  for (i in 1:n1)
  {
    for (j in 1:n2)
    {
      if ((1/(1+t((data2[j]-data1_mean_est))%*%solve(data1_cov_est)%*%(data2[j]-data1_mean_est)))>=dep1_new[i])
      {
        fre2[i]<-fre2[i]+1
      }
      else
      {
        fre2[i]<-fre2[i]
      }
    }
  }
  J2_hat<-sum(fre2)/(n1*n2) #estimated value of the containment index J2
  J_hat<-4*J1_hat*J2_hat    #estimated value of the overlap index J

  alpha_level<-0.05 #(1-alpha_level) is the confidence coefficient
  B<-2000           #number of bootstrap replicates
  J1_hat_boot<-rep(NA,B)
  J2_hat_boot<-rep(NA,B)
  J_hat_boot<-rep(NA,B)
  for (b in 1:B)
  {
    set.seed(17*b)
    data1_boot<-rnorm(n1,data1_mean_est,sqrt(data1_cov_est))   #bootstrap sample from first population
    data2_boot<-rnorm(n2,data2_mean_est,sqrt(data2_cov_est))   #bootstrap sample from second population
    ##for the estimated values of the containment and overlap indices based on bootstrap samples
    data1_mean_est_boot<-mean(data1_boot)
    data1_cov_est_boot<-var(data1_boot)
    data2_mean_est_boot<-mean(data2_boot)
    data2_cov_est_boot<-var(data2_boot)
    dep1_boot<-rep(NA,n1)
    dep2_boot<-rep(NA,n2)
    fre1_boot<-rep(0,n2)
    fre2_boot<-rep(0,n1)
    for(i in 1:n1)
    {
      dep1_boot[i]<-1/(1+t((data1_boot[i]-data1_mean_est_boot))%*%solve(data1_cov_est_boot)%*%(data1_boot[i]-data1_mean_est_boot))
    }
    for(j in 1:n2)
    {
      dep2_boot[j]<-1/(1+t((data2_boot[j]-data2_mean_est_boot))%*%solve(data2_cov_est_boot)%*%(data2_boot[j]-data2_mean_est_boot))
    }
    dep1_sort_boot<-sort(dep1_boot, decreasing=TRUE)
    dep2_sort_boot<-sort(dep2_boot, decreasing=TRUE)
    for(j in 1:(n2))
    {
      for(i in 1:n1)
      {
        if ((1/(1+t((data1_boot[i]-data2_mean_est_boot))%*%solve(data2_cov_est_boot)%*%(data1_boot[i]-data2_mean_est_boot)))>=dep2_sort_boot[j])
        {
          fre1_boot[j]<-fre1_boot[j]+1
        }
        else
        {
          fre1_boot[j]<-fre1_boot[j]
        }
      }
    }
    J1_hat_boot[b]<-sum(fre1_boot)/(n1*n2)         #estimated value of J1 based on bootstrap samples
    for(i in 1:(n1))
    {
      for(j in 1:n2)
      {
        if ((1/(1+t((data2_boot[j]-data1_mean_est_boot))%*%solve(data1_cov_est_boot)%*%(data2_boot[j]-data1_mean_est_boot)))>=dep1_sort_boot[i])
        {
          fre2_boot[i]<-fre2_boot[i]+1
        }
        else
        {
          fre2_boot[i]<-fre2_boot[i]
        }
      }
    }
    J2_hat_boot[b]<-sum(fre2_boot)/(n1*n2)         #estimated value of J2 based on bootstrap samples
    J_hat_boot[b]<-4*J1_hat_boot[b]*J2_hat_boot[b] #estimated value of J based on bootstrap samples
  }
  q1_J1<-quantile((J1_hat_boot-J1_hat),(alpha_level/2),names=FALSE)
  q2_J1<-quantile((J1_hat_boot-J1_hat),(1-(alpha_level/2)),names=FALSE)
  q1_J2<-quantile((J2_hat_boot-J2_hat),(alpha_level/2),names=FALSE)
  q2_J2<-quantile((J2_hat_boot-J2_hat),(1-(alpha_level/2)),names=FALSE)
  q1_J<-quantile((J_hat_boot-J_hat),(alpha_level/2),names=FALSE)
  q2_J<-quantile((J_hat_boot-J_hat),(1-(alpha_level/2)),names=FALSE)

  ##bootstrap confidence interval for J1
  lower_J1<-J1_hat+q1_J1          #lower limit of the bootstrap CI of J1
  upper_J1<-J1_hat+q2_J1          #upper limit of the bootstrap CI of J1

  ##bootstrap confidence interval for J2
  lower_J2<-J2_hat+q1_J2          #lower limit of the bootstrap CI of J2
  upper_J2<-J2_hat+q2_J2          #upper limit of the bootstrap CI of J2

  ##bootstrap confidence interval for J
  lower_J<-J_hat+q1_J             #lower limit of the bootstrap CI of J
  upper_J<-J_hat+q2_J             #upper limit of the bootstrap CI of J

  ##result
  cat("*********************************************\n")
  cat("Estimated value of J1, J2 and J are:", J1_hat, J2_hat, J_hat)
  cat("\n Lower and upper limits of the 95% bootstrap confidence interval of J1 are:", lower_J1, upper_J1)
  cat("\n Lower and upper limits of the 95% bootstrap confidence interval of J2 are:", lower_J2, upper_J2)
  cat("\n Lower and upper limits of the 95% bootstrap confidence interval of J are:", lower_J, upper_J)
  cat("\n*********************************************")
}
################################################################################



