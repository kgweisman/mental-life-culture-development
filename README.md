# Similarities and Differences in Concepts of Mental Life Among Adults and Children in Five Cultures

**Authors**: Kara Weisman ([website](http://kgweisman.github.io/)), Cristine H. Legare, Rachel E. Smith, Vivian A. Dzokoto, Felicity Aulino, Emily Ng, John C. Dulin, Nicole Ross-Zehnder, Joshua D. Brahinsky, & Tanya M. Luhrmann

**Abstract**: How do concepts of mental life vary across cultures? By asking simple questions about humans, animals, and other entities—e.g., *Do beetles get hungry? remember things? feel love?*—we reconstructed concepts of mental life from the bottom up among adults (N=711) and children (ages 6-12y, N=693) in the US, Ghana, Thailand, China, and Vanuatu. This revealed a cross-cultural and developmental continuity: In all sites, among both adults and children, cognitive abilities traveled separately from bodily sensations, suggesting that a mind-body distinction is common across diverse cultures and present by middle childhood. Yet there were substantial cultural and developmental differences in the status of social-emotional abilities—as part of the body, part of the mind, or a third category unto themselves. Such differences may have far-reaching social consequences, while the similarities identify aspects of human understanding that may be universal.


This repo includes analyses of the quantitative data from these studies, which were part of the [Mind and Spirit Project](https://themindandspiritproject.stanford.edu/#Home).

**Datasets** for adults and children (6-12y) from each of the five field sites are available [here](https://github.com/kgweisman/mental-life-culture-development/tree/master/data); demographic information is available [here](https://github.com/kgweisman/mental-life-culture-development/tree/master/demographics). Scripts for cleaning and anonymization can be found in [data_preparation.nb.html](http://htmlpreview.github.io/?https://github.com/kgweisman/mental-life-culture-development/blob/master/analyses/data_preparation.nb.html) and [demo_preparation.nb.html](http://htmlpreview.github.io/?https://github.com/kgweisman/mental-life-culture-development/blob/master/analyses/demo_preparation.nb.html).

**Analysis scripts** include:
- the **primary analyses** described the main text (exploratory factor analyses using Pearson correlations and oblique transformations): [efa_oblique.nb.html](http://htmlpreview.github.io/?https://github.com/kgweisman/mental-life-culture-development/blob/master/analyses/efa_oblique.nb.html))
- a secondary analysis using orthogonal rotations instead of oblique transformations: [efa_orthogonal.nb.html](http://htmlpreview.github.io/?https://github.com/kgweisman/mental-life-culture-development/blob/master/analyses/efa_orthogonal.nb.html))
- a secondary analysis recoding answers of "kinda" as equivalent to "yes" and using tetrachoric correlations: [efa_tetrachoric.nb.html](http://htmlpreview.github.io/?https://github.com/kgweisman/mental-life-culture-development/blob/master/analyses/efa_tetrachoric.nb.html))
- a secondary analysis dropping participants who gave the same answer (e.g., "yes" or "no") on every trial: [efa_drop_subjects.nb.html](http://htmlpreview.github.io/?https://github.com/kgweisman/mental-life-culture-development/blob/master/analyses/efa_drop_subjects.nb.html))
- a secondary analysis using principal components analysis rather than exploratory factor analysis: [pca.nb.html](http://htmlpreview.github.io/?https://github.com/kgweisman/mental-life-culture-development/blob/master/analyses/pca.nb.html))
- a secondary analysis of demographic variability in covariance patterns: [demo_comparison.nb.html](http://htmlpreview.github.io/?https://github.com/kgweisman/mental-life-culture-development/blob/master/analyses/demo_comparison.nb.html))

To view the results of an analysis in an HTML file, download the R Notebook (extension: .nb.html) to a folder on your computer and re-open it (from that folder) in a web browser -- or use the [htmlpreview.github.io](http://htmlpreview.github.io/) links provided. To view the code, click the "View code" button in the upper right. To view and manipulate the code, download the R Markdown file (extension: .Rmd) and open it in RStudio.

**Programming environment**: All analyses were conducted in R (version 4.0.0); platform: x86_64-apple-darwin17.0 (64-bit); running under: macOS Catalina 10.15.7.

The analyses were built using the following packages:
- tidyverse (version 1.3.0) 
- lubridate (version 1.7.8)
- readxl (version 1.3.1)
- psych (version 1.9.12.31)
- cowplot (version 1.0.0)
- reshape2 (version 1.4.4)
- sjstats (version 0.18.0)
- lsa (version 0.73.2)
- langcog (version 0.1.9001; available at https://github.com/langcog/langcog-package)
- betareg (version 3.1.3)
- lme4 (verison 1.1.23)
- lmerTest (version 3.1.2)
