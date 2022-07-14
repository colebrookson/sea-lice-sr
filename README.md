# Effect of Sea Lice on Wild Pacific Salmon 

This repo is home to all data (temporarily) and eventually code/docs, for a coast-wide analysis of the effect of sea lice from wild and farmed salmon on the dynamics of wild pacific salmon. 

As this work has been made possible by the Broughton Aquaculture Transition Initiative through the Broughton Area First Nations (i.e. 'Namgis, Mamalilikulla, Kwikwasut'inuxw Haxwa'mis First Nations), all products here including documentation, code, reports, recommendations and manuals developed for the Broughton Area First Nations will be owned by, belong to, and be the property of the Broughton Area First Nations. 

Please feel free to use and borrow from this code base according to the licence below. This code and the associated published paper will have a DOI pasted in this repository when possible.

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

### To add Overleaf Document to Github Repo

link that inspired this: [https://stackoverflow.com/questions/52961374/how-to-merge-a-overleaf-project-to-an-already-existing-github-repo](https://stackoverflow.com/questions/52961374/how-to-merge-a-overleaf-project-to-an-already-existing-github-repo)

You can use the git address provided by overleaf to link to your repo as a submodule. 

```
git submodule add --force --name manuscript https://git.overleaf.com/62c1a311179c78326977844d manuscript
```

Then, to update, you can simply go into the manuscript directory and run `git fetch`
