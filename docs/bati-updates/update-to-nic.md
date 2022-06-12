---
header-includes:
  - \usepackage{enumitem}
  - \setlistdepth{20}
  - \renewlist{itemize}{itemize}{20}
  - \renewlist{enumerate}{enumerate}{20}
  - \setlist[itemize]{label=$\cdot$}
  - \setlist[itemize,1]{label=\textbullet}
  - \setlist[itemize,2]{label=--}
  - \setlist[itemize,3]{label=*}
output:
  markdown::pdf_document:
      keep_tex: yes
---

# Update on BATI Contract

## Data Acquisition

So far, we have successfully attained all the data required to run analyses on coho and pink salmon. The pink data took quite some time, but we finally received it at the beginning of June from Pieter Van Will at DFO. This means that we have everything in hand to move forward with the stock-recruit analysis for pink and coho. 

The chum data we are still waiting on from Pieter, and hopefully that will come in soon, but it isn't looking lately that we will receive it before July. 

## Analysis

So far, we have successfully replicated the component of the analysis that looks at the correlation between lice on farmed fish and on wild fish. We were able to use the data from Salmon Coast Field Station on numbers of lice on wild fish, along with the data on the number of lice on farmed fish to see how those two patterns relate. 

Given that some farms likely contribute far more lice to the number of lice on wild juvenile salmon, we compared three different groupings of the farms. We looked at the relationship of lice on wild juveniles to a) the number of lice on all farms, b) the number of lice on the farms in the Knight-Tribune corridor, and c) the number of lice on three focal farms - Humphrey, Sergeant, and Doctors. 

We show that the number of lice on wild juveniles is best predicted by the number of lice on all farms. 

![Farm Comparison](/figs/wild-to-farm-models-comparison.png)
**Figure 1:** A comparison of the relationship between the number of lice on wild salmon (on a log10 scale), and the number of lice on the three sets of farm comparisons (also a log10 scale).

Given these results, we are able to use our model to estimate the number of lice on the 

## Next Steps
