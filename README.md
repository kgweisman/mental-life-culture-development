# Concepts of Mental Life Among Adults and Children in Five Cultures

Authors: Kara Weisman, Cristine H. Legare, Rachel E. Smith, Vivian A. Dzokoto, Felicity Aulino, Emily Ng, John C. Dulin, Nicole Ross-Zehnder, Joshua D. Brahinsky, & Tanya M. Luhrmann

How do concepts of mental life vary across cultures? By asking simple questions about humans, animals, and other entities—e.g., *Do beetles get hungry? remember things? feel love?*—we reconstructed concepts of mental life from the bottom up among adults (N=711) and children (ages 6-12y, N=693) in the US, Ghana, Thailand, China, and Vanuatu. This revealed striking cross-cultural and developmental continuity: In all sites, among both adults and children, cognitive abilities traveled separately from bodily sensations, suggesting that a mind-body distinction is common across diverse cultures and present by middle childhood. Yet there were substantial cultural and developmental differences in how social-emotional abilities fit in—as part of the body, the mind, or a third category unto themselves. Such differences have far-reaching social consequences, while the similarities identify aspects of human thought that may be universal.

This repo includes analyses of the quantitative data from these studies, which were part of the [Mind and Spirit Project](https://themindandspiritproject.stanford.edu/#Home).

**Analysis scripts** include both the primary results described the main text (exploratory factor analyses using Pearson correlations and oblique transformations) and many second analyses described or briefly referred to in the supplement. To view the results of an analysis in an HTML file, download the R Notebook (extension: .nb.html) to a folder on your computer and re-open it (from that folder) in a web browser -- or use the [htmlpreview.github.com](htmlpreview.github.com) links provided below. To view and manipulate the code, download the R Markdown file (extension: .Rmd) and open it in RStudio.

**Dependencies**: These analyses were conducted using the following R packages:
- tidyverse (built using version 1.3.0) 
- lubridate (built using version 1.7.8)
- readxl (built using version 1.3.1)
- psych (built using version 1.9.12.31)
- cowplot (built using version 1.0.0)
- reshape2 (built using version 1.4.4)
- sjstats (built using version 0.18.0)
- lsa (built using version 0.73.2)
- langcog (built using version 0.1.9001; available at https://github.com/langcog/langcog-package)
- betareg (built using version 3.1.3)
- lme4 (built using verison 1.1.23)
- lmerTest (built using version 3.1.2)
