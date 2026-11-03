# Maintaining the website

Build the site locally from the package root:

```r
pkgdown::build_site()
```

The site is written to `docs/`. To serve the existing output locally, install
the optional `nanonext` package and run `pkgdown::preview_site()`. Stop the
server with `pkgdown::stop_preview()`.

The package is not on CRAN, so the site uses pkgdown's unreleased development
mode and has one version. CI builds the site with read-only permissions, then
publishes the build artifact to the `gh-pages` branch.
