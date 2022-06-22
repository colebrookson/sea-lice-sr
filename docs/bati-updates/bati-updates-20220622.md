---
theme: default
_class: lead
paginate: true
marp: false
backgroundColor: #fff
style: |
  img {background-color: transparent!important;}
  a:hover, a:active, a:focus {text-decoration: none;}
  header a {color: #ffffff !important; font-size: 30px;}
  footer {color: #148ec8;}
---

# **Updates on Initial Stock-Recruit Model Results**

*Presenter:* Cole Brookson
*Date:* 2022-02-17

---

# Model

Followed the form in Peacock (2013)

$$ \text{ln}[R_{i,t}/N_{i,t-2}] = r - b_iN_{i,t-2} - cW_{a,t-1} + \theta_t + \theta_{a,t} + \epsilon_{i,t}$$

* Fit in `glmmTMB`
  * Null model: `survival ~ scale(S):population_name + 
                                (1|year/area)`
  * Alternative model: `survival ~ scale(S):population_name + 
                               scale(lice) + (1|year/area)`
* Reason for using `scale()` on the predictors is the models didn't converge without it - non-positive-definite Hessian
* Final dataset (only kept populations with > 20 stock-recruits per populations):
  * 77 populations (even/odd)
  * 1752 S-R pairs
  * 45 rivers

---
# Results

Comparison between null model and alternative model:

![bg right:80% height:90%](./figs/sr-model-anova.png)

1. No clear difference between the null model and the alternative model
   * Note - cannot test the model differences without using the `scale()` in `glmmTMB` but can force them to fit `lme4` - however there was still no difference
2. **Model parameters** for the growth rate, $r$ (Intercept) = 0.19218, and the strength of the relationship betweenpink salmon survival and lice on wild juvenile salmon, $c$ = 0.09217

---

# Why is This Different Than Stephs Results

I think there are some actually very small errors in the code that introduced some problems (**I have not fully tested this**)

---

# Problem 1 - Sample size 

In the paper (and in the final dataset generated in the code), there are: 
   * 179 populations
   * 2307 S-R pairs
   * 99 rivers
   * 
But, I think this is actually too large of a dataset. There is code to exclude populations with <20 S-R pairs: 
![bg right:50% height:60%](./figs/less-than-20-pairs.png)


---

# Why Git from the command line? 

* It's the only place you can run *all* Git commands 
* If you know the command line version you can probably figure out a GUI version - the opposite is not necessarily true 
* You might have a preference of GUI, but *all* users can use command line tools
* Interacting with servers needs to be done via command line, so you might as well learn how to do it on your own machine 
* Language-specific plug-ins (i.e. Git for RStudio) force you to open the IDE for that language every time you need to make a change to a file, even if it's not in that langauge 

---

# Cloud-based Git repository hosting service (GitHub)

* A for-profit company that hosts Git repositories
* Free to use for public repositories (makes it *very* popular for open-source projects)
* Provides a nice interface for viewing your repositories contents
* Allows you to publish items with DOIs (links with Zenodo for this) 

---
# Important Concept: Branches 

* When you are making lots of changes, you don't necessarily want to work on the "stable" branch
* This is especially important when collaborating with others who rely on having a working code base 

---
# Important Concept: Merging 

* Merging is what allows us to make the changes that happened on the "feature branch" present on the main branch, once we're sure we like them 
* This can get complicated with large numbers of files, but the great thing about Git is you can **always** go back if you mess up!

---
# Important Concept: Reverting 

* We might make mistakes, and it's important to know how to "undo" those mistakes 
* There are often two scenarios: 
  * You want to keep some of the work you did since the "bad" commit
  * You don't want to keep any of it (usually one or two commits back)
  * 
