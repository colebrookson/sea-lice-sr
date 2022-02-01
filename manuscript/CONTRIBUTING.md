# How to contribute?

## Requirements

0.1. A free, personal [Github Account](https://github.com/join?source=header-home) is necessary to contribute to this project.

0.2. Become familiar with the concepts and functions of repository, issue, issue template, manuscript source, and pull-request. Visit the [Getting started](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/USAGE.md#getting-started) section from the [USAGE.md](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/USAGE.md) for a brief summary on these main concepts!

0.3. Write a few words about yourself in the [Self-Introductions](https://github.com/SORTEE-Github-Hackathon/manuscript/discussions/4) discussion section. *Copy and paste the following chunk and fill in with your information*:

```
*My Name is*: 

*What do I do*: 

*How can I or with what I would like to contribute*: 

*What is the most trepidation that I have*:
```

## Contributing with writing

1. To situate yourself, access the [current document](https://sortee-github-hackathon.github.io/manuscript/v/latest/index.html) and/or its corresponding sections within the [content](https://github.com/SORTEE-Github-Hackathon/manuscript/tree/main/content) directory;

2. It is **essential** that you **always keep your version up-to-date** (_i.e._ always *fetch* and *pull* from the origin), so you are certain to not provide changes to a previous version of the manuscript. The easiest ways to being modifying are to:
   1. [Fork this repository](https://github.com/SORTEE-Github-Hackathon/manuscript/) to your Github profile, and [Contribute](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/CONTRIBUTING.md) your modifications from there. *Remember to always fetch upstream to keep your repository up-to-date*.; or,
   2. [Clone this repository](https://github.com/SORTEE-Github-Hackathon/manuscript/) to your computer, create and work from your own [branch](https://github.com/SORTEE-Github-Hackathon/manuscript) and push your committed modifications through [pull requests](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/USAGE.md#pull-requests). *Always fetch and pull from the origin when you work, so your local copy is updated.*

3. Write text and propose changes to a file by making a pull request:
   1. **Write a sentence per line** and create new paragraphs with two returns;
   2. It is *highly* encouraged to commit frequently, so you can finely track and, if necessary, revert changes.
   3. Add relevant descriptive names to commits and pull requests;
      1. Preferred: `Adding a sentence on how the use of Github can help instructors achieve learning objectives`;
      2. Not preferred: `Update 1`, `Modification`, `asdas blabla`.
   4. For further information about `Markdown` formatting, access the [`USAGE.md`](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/USAGE.md) document.

4. All pull requests must be reviewed before being merged to the `main` repository. You may **assign reviewers** if you would like someone to specifically review your pull requests. See the [Reviewing pull requests](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/CONTRIBUTING.md#reviewing-writing) section for more information;

5. Would you like to point to something important, make a question or a suggestion? Begin a conversation in the [Discussions](https://github.com/SORTEE-Github-Hackathon/manuscript/discussions);

6. Fill in your author information by initiating a pull request on the [`metadata.yaml`](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/content/metadata.yaml) file (the authorship order will be decided later). (_For the purposes of this manuscript, authors must accummulate contributions that go beyond reviewing typos over the lifetime of this project._)
   - Authorship contributions are categorized following the guidelines from the [CRediT Taxonomy](https://casrai.org/credit/) and the [International Committee of Medical Journal Editors](http://www.icmje.org/recommendations/browse/roles-and-responsibilities/defining-the-role-of-authors-and-contributors.html).
  


## Reviewing writing

All pull requests should be reviewed by, at least, two other contributors. Pull request reviews can be open, so that anyone can review them. However, you may assign specific reviewers to them.

When reviewing a pull request, one needs to consider:

1. Whether the pull request passed all Manubot checks. Green means good. Red means that there are issues that must be solved *before* merging the text to the main manuscript. [See here for more information](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/USAGE.md#pull-requests).
2. Verify if there are newly cited literature? If yes, are they available in the [Relevant Literature](https://github.com/SORTEE-Github-Hackathon/manuscript/discussions/categories/relevant-papers) section? If not, request the author of the pull request to include it there or add it yourself. The instructions to do it are just below, in the [Suggesting a paper](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/CONTRIBUTING.md#suggesting-a-paper) section.
3. To the extent of your expertise, do you think that there are any ambiguities, oversimplifications or subjects that should be clarified?
4. Will the text be easy for someone from any scientific discipline to understand?

If you feel that there are issues in the pull request that needs to be broadly discussed, you should write a comment below and write an issue, while tagging the pull request you have made.

## Suggesting a paper

1. Choose a paper;
2. Search for its title or its DOI in the [Relevant Papers](https://github.com/SORTEE-Github-Hackathon/manuscript/discussions/categories/relevant-papers) discussion section;
3. If the paper you would like to suggest has been already suggested, contribute to the discussion by adding your view about the research piece;
3. If the paper is not already discussed, create a [New Discussion](https://github.com/SORTEE-Github-Hackathon/manuscript/discussions/new?category=relevant-papers) with the paper title as **Title**, and with the following template filled in:

```
<!--Hi there! Please use this template as a guide for what information about your suggested paper to include. It is fine to leave fields blank so that other contributors can fill them in later, or to add fields that you think are important.-->

### Title: Include the manuscript's title

Study Link: Include the https:// link that brings us to the page of the manuscript
Citation: `@doi:replace`

> Citation must follow the [Manubot-style citation](https://github.com/SORTEE-Github-Hackathon/manuscript/blob/main/USAGE.md#citations). Leave it in blank, if unsure.

Suggested keywords that help identify the relevance of this paper to Ecology and Evolution:
* Keyword 1 (replace me, copy and paste more than three if needed)
* Keyword 2 (replace me, copy and paste more than three if needed)
* Keyword 3 (replace me, copy and paste more than three if needed)

### Which areas of expertise are particularly relevant to the paper?
<!-- Mark with an x the relevant areas, i.e. [x]. -->
- [x] ecology and/or evolution;
- [ ] biostatistics;
- [ ] informatics and computational research;
- [ ] open science and reproducibility;
- [ ] other: replace_with_your_suggestion.

# Summary

### Suggested questions to answer about each paper:
- What is the general main finding or takeaway of the paper?
- What did they analyse and how did they do it?
- What does this paper suggest to improve the issue with reproducibility in science?
- Do you have any concerns about methodology or the interpretation of these results beyond this analysis?

### Any comments or notes?
```

## Opening new discussions

Any type of discussion related to this project is welcome! You can visit the [Discussions](https://github.com/SORTEE-Github-Hackathon/manuscript/discussions) section of this repository and begin from there.

You can search for topics and even propose new categories!

We still have not established what should be an [Issue](https://github.com/SORTEE-Github-Hackathon/manuscript/issues) instead of a [Discussion](https://github.com/SORTEE-Github-Hackathon/manuscript/discussions) topic. 

We can consider that issues are tasks that need to be addressed, are *not* permanent, and thus are made to be closed. We should discuss about this (!).

> Important: _If you would like to provide changes to files that are not related to the `content` directory, please open a descriptive issue and only perform prospective changes through pull requests._
