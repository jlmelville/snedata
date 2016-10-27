# snedata
SNE Dataset Functions for R

This package provides functions for generating simple simulation datasets 
for use in Stochastic Neighbor Embedding and related dimensionality reduction
methods, similar to those used by Lee and co-workers in their JSE papers.

Recently, Martin Wattenberg, Fernanda Viégas and Ian Johnson published an 
interactive article [How to use t-SNE Effectively](http://distill.pub/2016/misread-tsne/).
The JavaScript functions used to create the simulation datasets
(which can also be found at https://github.com/distillpub/post--misread-tsne),
have been translated into R and are also hosted in this package.

Additionally, if you have the [RnavGraphImageData](https://cran.r-project.org/web/packages/RnavGraphImageData/index.html)
package installed, there are also functions to convert the Olivetti and Frey 
faces datasets into a row-based dataframe and functions to visualize them.

### Installing:
```R
install.packages("devtools")
devtools::install_github("jlmelville/snedata")
```

### Documentation:
```R
package?snedata # lists all the functions
?gaussian_data # contains links to all the other distill.pub simulation functions
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

# Load the Olivetti faces dataset with each image as a row
olivetti <- olivetti_faces()
# Show the second pose of the first face
show_olivetti_face(olivetti, 1, 2)
```

### See also
I have similar R packages for the 
[COIL-20](https://github.com/jlmelville/coil20) and 
[MNIST Digit](https://github.com/jlmelville/mnist) datasets.
For doing an embedding, you could give 
[sneer](https://github.com/jlmelville/sneer) a go.

### License
[MIT License](http://opensource.org/licenses/MIT).
