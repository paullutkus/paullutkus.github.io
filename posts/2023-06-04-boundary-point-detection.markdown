---
title: Boundary Point Detection Algorithms
---

## Introduction ##
My current project involves sampling a set of points on which I'll learn a 
control barrier function. I need to automate the detection of boundary
points in the sampled set, which is not guaranteed to be convex. I'm using
this post to document my experimentation with a number of algorithms that
could solve this problem.

## Alpha Shape: ##
The concept of an 'alpha shape' is a nonconvex 
generalization of the convex hull.
