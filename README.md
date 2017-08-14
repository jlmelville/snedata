# snedata
SNE Dataset Functions for R

This package provides functions for generating simple simulation datasets 
for use in Stochastic Neighbor Embedding and related dimensionality reduction
methods, similar to those used by Lee and co-workers in their 
[JSE](http://dx.doi.org/10.1016/j.neucom.2012.12.036) and [multi-JSE](http://dx.doi.org/10.1016/j.neucom.2014.12.095) papers.

Recently, Martin Wattenberg, Fernanda Viégas and Ian Johnson published an 
interactive article [How to use t-SNE Effectively](http://distill.pub/2016/misread-tsne/).
The JavaScript functions used to create the simulation datasets
(which can also be found at https://github.com/distillpub/post--misread-tsne),
have been translated into R and are also hosted in this package.

Additionally, if you have the [RnavGraphImageData](https://cran.r-project.org/web/packages/RnavGraphImageData/index.html)
package installed, there are also functions to convert the Olivetti and Frey 
faces datasets into a row-based data frame and functions to visualize them.

### Installing:
```R
install.packages("devtools")
devtools::install_github("jlmelville/snedata")
```

### Documentation:
```R
package?snedata # lists all the functions
?snedata::gaussian_data # contains links to all the other distill.pub functions
```

### Examples
```R
# 3000 points sampled from the surface of a sphere
sphere3000 <- sphere(n = 3000)

# 1500 points sampled from a toroidal helix with 30 coils:
helix1500 <- helix(n = 1500, nwinds = 30)

# 1500 points from a filled sphere:
ball1500 <- ball(n = 1500)

# 1000 points from a "Swiss Roll" distribution:
swiss1000 <- swiss_roll(n = 1000)

# 1000 points from a five-dimensional gaussian:
gauss1000 <- gaussian_data(n = 1000, dim = 5)

# Load RnavGraphImageData
library(RnavGraphImageData)

# Load the Frey faces dataset with each image as a row
frey <- frey_faces()
# Display the first pose
show_frey_face(frey, 1)

# PCA scores plot, with color indicating the frame index
frey_pca <- prcomp(frey[, -561], retx = TRUE, rank. = 2)
plot(frey_pca$x, col = frey$color, pch = 16, cex = 0.75)

# Load the Olivetti faces dataset with each image as a row
olivetti <- olivetti_faces()
# Show the second pose of the first face
show_olivetti_face(olivetti, 1, 2)

# Generate datasets similar to those used in the main text of "How to Use t-SNE Effectively"
misread_tsne <- list(
	two_clusters = snedata::two_clusters_data(n = 50, dim = 2),
	two_different_sized_clusters = snedata::two_different_clusters_data(n = 75, dim = 2),
	three_clusters_50 = snedata::three_clusters_data(n = 50, dim = 2),
	three_clusters_200 = snedata::three_clusters_data(n = 200, dim = 2),
	gaussian_cloud = snedata::gaussian_data(n = 500, dim = 100),
	ellipsoidal_gaussian_cloud = snedata::long_gaussian_data(n = 100, dim = 50),
	two_long_linear_clusters = snedata::long_cluster_data(n = 75),
	cluster_in_cluster = snedata::subset_clusters_data(n = 75, dim = 50),
	linked_rings = snedata::link_data(n = 100),
	trefoil_knot = snedata::trefoil_data(n = 150)
)
```

### See also
I have similar R packages for the 
[COIL-20](https://github.com/jlmelville/coil20) and 
[MNIST Digit](https://github.com/jlmelville/mnist) datasets.
For doing an embedding, you could give 
[sneer](https://github.com/jlmelville/sneer) a go.

### License
[MIT License](http://opensource.org/licenses/MIT).
