# Download 20 Newsgroups

Downloads the 20 Newsgroups dataset, which contains approximately 20,000
newsgroup documents from 20 different newsgroups. The distribution is
approximately balanced.

## Usage

``` r
download_twenty_newsgroups(
  subset = "all",
  verbose = FALSE,
  tmpdir = NULL,
  cleanup = TRUE,
  timeout = 1800
)
```

## Format

A data frame with 6 variables:

- `Id`: A stable source-derived identifier containing the subset,
  newsgroup, and original filename, e.g. `train/alt.atheism/49960`.

- `FileId`: The original source filename as a character value. These are
  *not* unique across subsets and newsgroups.

- `Text`: The full text of the message including any header, footer, and
  quotes. Newlines are preserved.

- `Subset`: A factor with two levels: `train` and `test`, indicating
  whether the document is from the training or test subset.

- `Label`: A factor with levels `0` through `19`, identifying the
  newsgroup using the canonical ordering below.

- `Newsgroup`: A factor with the 20 newsgroups as levels in the
  canonical ordering below.

The labels correspond to:

- `0`: alt.atheism

- `1`: comp.graphics

- `2`: comp.os.ms-windows.misc

- `3`: comp.sys.ibm.pc.hardware

- `4`: comp.sys.mac.hardware

- `5`: comp.windows.x

- `6`: misc.forsale

- `7`: rec.autos

- `8`: rec.motorcycles

- `9`: rec.sport.baseball

- `10`: rec.sport.hockey

- `11`: sci.crypt

- `12`: sci.electronics

- `13`: sci.med

- `14`: sci.space

- `15`: soc.religion.christian

- `16`: talk.politics.guns

- `17`: talk.politics.mideast

- `18`: talk.politics.misc

- `19`: talk.religion.misc

`Newsgroup` contains the corresponding names.

There are 11,314 items in the `train` dataset and 7,532 items in the
`test` for a total of 18,846 items if you choose `subset = "all"`.

## Arguments

- subset:

  A string specifying which subset of the dataset to download and
  process. Acceptable values are `"train"` for the training set,
  `"test"` for the test set, and `"all"` for both sets combined. Default
  is `"all"`.

- verbose:

  If `TRUE`, log progress of download, extraction and processing.

- tmpdir:

  A string specifying the parent directory for a dedicated download and
  extraction work directory. If `NULL` (default), a temporary parent
  directory is used. If a path is provided and does not exist, it will
  be created.

- cleanup:

  A logical flag indicating whether to delete the downloaded and
  extracted files after processing. If `TRUE` *and* `tmpdir` was created
  by this function, `tmpdir` will be deleted after processing. Default
  is `TRUE`.

- timeout:

  Minimum download timeout in seconds. The default is 30 minutes; a
  larger existing global R timeout is preserved.

## Value

Data frame containing 20 Newsgroups Data.

## Details

To do any analysis on this text, you will want to use tools from
packages such as [tm](https://cran.r-project.org/package=tm) and
[tidytext](https://cran.r-project.org/package=tidytext). The files are
read as `latin1` encoding, but there can still be some odd control codes
in some of the messages.

## References

Lang, K. (1995). Newsweeder: Learning to filter netnews. In *Proceedings
of the Twelfth International Conference on Machine Learning* 1995 (pp.
331-339). Morgan Kaufmann.

## See also

<https://qwone.com/~jason/20Newsgroups/>

Chapter 9 of [Tidy Text Mining with
R](https://www.tidytextmining.com/usenet) for a case study using the
same dataset.

## Examples

``` r
if (FALSE) { # \dontrun{

# Download and process the training set
ng_train <- download_twenty_newsgroups(subset = "train")

# Download and process both training and test sets, with verbose output
ng_all <- download_twenty_newsgroups(subset = "all", verbose = TRUE)

# Download and process the test set, using a specific directory and enabling
# cleanup
ng_test <- download_twenty_newsgroups(
  subset = "test",
  tmpdir = "path/to/dir", cleanup = TRUE
)
} # }
```
