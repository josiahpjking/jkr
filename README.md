# jkr
Package of various functions which will slowly grow.. 

## To install
> devtools::install_github("josiahpjking/jkr")

## things included:

### For plotting time courses of eye and mouse tracking data
make_tcplotdata, tcplot

### equivalent of SAS's SUM (basically rowSums with na.rm=T, except for when entire row of NAs, which returns NA, not 0)
rowSums_na

### quick tables with prop.test for groups. and confints
create_proptable

### and others..
