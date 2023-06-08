---
title: Boundary Point Detection Algorithms
---

## [Introduction]{.underline} ##

My current project involves sampling a set of points on which I'll learn a 
control barrier function. I need to automate the detection of boundary
points in the sampled set, which is not guaranteed to be convex. I'm using
this post to document my experimentation with a number of algorithms that
could solve this problem.
\
\

## [Alpha Shape]{.underline} ##

The concept of an 'alpha shape' is a nonconvex generalization of the convex 
hull. Most theorems and proofs regarding the alpha shape of a set of points
necessitate placing the points in 'general position'.


<section class="indent">
### General Position ###

Considering points that are in general position is a way to avoiding edge 
cases when constructing proofs and programs. In $n$-dimensional space
</section>

