# house-price-model
code for the paper:

Liu, B., Mavrin, B., Niu, D., & Kong, L. (2016, December). House Price Modeling over Heterogeneous Regions with Hierarchical Spatial Functional Analysis. In Data Mining (ICDM), 2016 IEEE 16th International Conference on (pp. 1047-1052). IEEE.

Data came from a NON OPEN source, so I can't post it here.

## 
 1. fdaPDE_iterative_backFit.R:
 
 Abstract:
Online real-estate information systems such as Zil- low and Trulia have gained increasing popularity in recent years. One important feature offered by these systems is the online home price estimate through automated data-intensive computation based on housing information and comparative market value analysis. State-of-the-art approaches model house prices as a combination of a latent land desirability surface and a regression from house features. However, by using uniformly damping kernels, they are unable to handle irregularly shaped regions or capture land value discontinuities within the same region due to the existence of implicit sub-communities, which are common in real-world scenarios. In this paper, we explore the novel application of recent advances in spatial functional analysis to house price modeling and propose the Hierarchical Spatial Functional Model (HSFM), which decomposes house values into land desirability at both the global scale and hidden local scales as well as the feature regression component. We propose statistical learning algorithms based on finite-element spatial functional analysis and spatial constrained clustering to train our model. Extensive evaluations based on housing data in a major Canadian city show that our proposed approach can reduce the mean relative house price estimation error down to 6.60%.

 2. kmeans.R:
 
Clustering for the house price data.

 3. Boundary.m:
 
Matlab code for the compuation of the $\alpha$-shape for house clusters.

 4. icdm16:
 
Orignal publication.

Note: data is not available for now.
