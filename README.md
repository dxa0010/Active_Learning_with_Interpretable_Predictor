# predict error reduction

This code is for reproducing the results in
“Active Learning with Interpretable Predictor”, Y. Taguchi, K. Kameyama, H. Hino, IJCNN2019.

## setup

You have to Download dataset.
We use 5 datasets here.
[wine quality](http://archive.ics.uci.edu/ml/datasets/wine+quality),
[bike](http://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset),
[CASP](https://archive.ics.uci.edu/ml/datasets/Physicochemical+Properties+of+Protein+Tertiary+Structure),
[housing](http://archive.ics.uci.edu/ml/machine-learning-databases/housing/),
[Freid](https://www.rdocumentation.org/packages/tgp/versions/2.4-14/topics/friedman.1.data)

If you get these files, you put on the "dataset" folder.

## experiment

If you done the setup, you can run the expriment.

```bash
Rscript exp_AL.R 1
```

c("loadCasp.R", "loadHousing.R", "loadFreid.R", "loadredWine.R", "loadwhiteWine.R", "loadBike.R")

The args number represent dataset. The 1 is CASP, 2 is housing, 4 is redwine, 5 is whitewine and 6 is bike. When we run above command the result is expressed in "env" folder.
You can run the plotmaker.R. Experimental result is in "env" folder


