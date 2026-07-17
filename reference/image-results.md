# Canonical image-dataset results

Image downloaders accept `as = "list"` to return one shared, shallow
structure. It has `data`, a matrix with one image per row; `meta`, a
data frame with matching rows; `image_dim`, named image dimensions;
`channel_order`; and `source`, a list containing the dataset name and
acquisition URL.

## Details

Metadata names are lower case: `label`, `description`, `split`, `id`,
`object`, and `pose` are used when applicable. Dataset-specific source
fields remain in `meta`. `split` records train/test identity explicitly.
The legacy data-frame result remains available with `as = "data.frame"`
and retains its historical column names.
