# snedata 0.0.0.9003

*July 16 2026*:

- New functions: `download_coil20()`, `download_coil100()`, and `show_coil_object()` for the
COIL-20 and COIL-100 image datasets. Downloading the data requires installing the optional `png`
package. This functionality formerly lived in the [coil20](https://github.com/jlmelville/coil20)
repo.
- Improved validation and cleanup of downloaded binary data and archives.
- Fixed QMNIST broken links due to project being archived.

# snedata 0.0.0.9002

*July 2 2026*: Version 0.0.9002 is released, featuring:

- a lot of tidying up, especially around cleaning up temp files and network connections.
- instead of a dataframe, for some larger datasets, you can now return a list containing the matrix
of the numeric data and any labels separately. Supply `as = "matrix"`.
- "new" datasets: The swiss roll and faces dataset from the
[Isomap paper](https://www.science.org/doi/10.1126/science.290.5500.2319), via
`download_isomap_swiss_roll` and `download_isomap_faces`, respectively. Also, a function to view
items in the faces dataset via `show_isomap_face`. Probably of historical interest only, but I
think we should preserve this history. Now vanished from the MIT webpage, one must rummage through
the Internet Archive, and have the optional [R.matlab](https://cran.r-project.org/package=R.matlab)
package installed. The faces dataset is stored as a Unix `compress` file, so you also need an
external `gzip` or `uncompress` command (I tried and failed to get this to work with the `archive`
package). Based on a [gist](https://gist.github.com/jlmelville/339dfeb80c3e836e887d70a37679b244) I
wrote a few years ago.
- new(er) dataset: `synthetic_hierarchical_data()` generates the 5 x 5 x 5 hierarchical Gaussian 
cluster data set used in the PaCMAP paper.

# snedata 0.0.0.9001 and earlier

*March 2 2025* New link to download the MNIST digits. The original pages on Yann LeCun's website
seem to have gone kaput. Fortunately <https://github.com/fgnt/mnist> maintained a mirror.

*November 25 2023* Added the [20 Newsgroups](http://qwone.com/~jason/20Newsgroups/) text dataset.
You'll need to look at packages like [tm](https://cran.r-project.org/package=tm) or
[tidytext](https://cran.r-project.org/package=tidytext) to process this into a form suitable for
embedding.

*May 14 2022* Added the `taspheres` function to generate the "spheres" dataset of high dimensional
spheres nested inside a larger sphere, as used in the
[Topological Autoencoders](https://arxiv.org/abs/1906.00722) paper.

*December 24 2021*. Added the S-curve with a hole and 2D curve datasets used in the
[PaCMAP](https://arxiv.org/abs/2012.04456) paper.

*December 21 2021*. Added the mammoth datasets from
[Understanding UMAP](https://pair-code.github.io/understanding-umap/).

*June 29 2019*. Added [QMNIST](https://github.com/facebookresearch/qmnist).

*March 6 2019*. Added [CIFAR-10](https://www.cs.toronto.edu/~kriz/cifar.html).

*February 23 2019*. Added [Small NORB](https://cs.nyu.edu/~ylclab/data/norb-v1.0-small/).

*December 14 2018*. Added [Kuzushiji-MNIST](https://github.com/rois-codh/kmnist).
