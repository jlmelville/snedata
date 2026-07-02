# snedata 0.0.0.9002

*July 1 2026* Version 0.0.9002 adds no new datasets, but did a lot of tidying up, especially around
cleaning up temp files and network connections. Also, instead of a dataframe, you can now return
a list containing the matrix of the numeric data and any labels separately. Supply `as = "matrix"`.

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
