---
title: Boundary Point Detection Algorithms
tags: Geometry
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
cases when constructing proofs and programs. In $n$-dimensional space, general
position is the 

</section>

\
\

## [Convex Hull]{.underline} ##

I've decided to use the 'Gift Wrapping' algorithm to compute the convex hull
since it generalizes nicely to computing the alpha shape as I'll explain.
Loosely, the procedure is as follows:

<u>

	Gift Wrapping

</u>

> 	1. Find the maximum point along a dimension 
>	   (wlog, highest point in Y), call it
>	   the 'first focus'.
> 	2. Repeat 3-5 until the 'new focus' is the 
>	   first focus:
>	3. Find the point which generates the minimum 
>	   nonnegative slope (m) when paired with the 
>	   focus.
>	4. Let this point be the 'new focus', and add 
> 	   it to the hull.
>	5. Rotate the dataset clockwise by arctan(m) 
> 	   around the new focus, so that it is 
>	   horizontally aligned with the previous 
>	   focus.

Since, for each hull point, we must iterate through each of the other $n$
points, this procedure has a time-complexity of $O(hn)$, where $h$ is the 
number of hull points.

<section class="indent">

### The Initial Maximum ###

As in &nbsp;<b>`step 1`</b>&nbsp;, &nbsp; we first find the point that 
is maximal in the $y$-dimension. Given a datasent of shape $(n,2)$, We'll 
create a copy of the data to process, and iterate through it to obtain its 
maximum in $y$.

```{.python}
# sort and remove point with maximum y
pt_max = pts[0,:]
pts_copy = [pt_max]
for idx, x in enumerate(pts[1:]):
	if x[1] > pt_max[1]:
		pt_max = x
	pts_copy.append(x)	
```

### The Central Loop ###

This section covers &nbsp;<b>`step 3`</b>&nbsp; and &nbsp;<b>`step 4`</b>&nbsp; 
and is primarily a do-while-loop that checks whether each new focus is the 
original (in which case, we've completed the hull). Recall that the new focus 
is the point which makes the minimum nonnegative slope with the previous
focus. 

A useful trick here, for initializing the minimum slope, is to set it
to &nbsp;`np.infty`&nbsp;, which is evaluates as larger than any numeric data 
type. This is an easy way to ensure that the first prospective point will 
always be chosen as the initial candidate.

Note that multiple points could generate the same slope with respect to the 
focus. In this case we want to pick the point that is furthest from the focus

In order to emulate a 'do-while', we use &nbsp;`while True:`&nbsp; to ensure 
that the loop always runs at least once, and then we evaluate the condition 
&nbsp;`focus != pt_max`&nbsp; at the end of the loop and break if it is false.

<b>Note:</b> &nbsp;`### THE REST ###`&nbsp; denotes what processing must be
done between iterations to ensure the slope conditions are continually valid,
and is what we'll discuss next.

```{.python}
# for each focus f, find min slope >= 0, 
# then rotate clockwise
f = pt_max
hull = []
total_theta = 0 # this will come in handy later
while True:
	m_min = np.infty
	for idx, pt in enumerate(pts_copy):
		# compute slope, update focus candidate 
		# 'fc' if slope is nonnegative and lesser 
		# than previous
		m = (f[1] - pt[1]) / (f[0] - pt[0])
		if m >= 0 and m < min_m:
			fc = idx
			min_m = m 
		# if same slope as previous candidate, take
		# whichever is further from current focus
		elif m == m_min:
		if np.linalg.norm(f - pt) 
		   > np.linalg.norm(f - pts_copy[fc]):
			fc = idx            
	# new focus is pt w/ minimum positive slope 
	# wrt prev focus, no need to consider in 
	# next iteration
	f = pts_copy[fc]
	hull.append(f)
	pts_copy.remove(fc)

	############
	# THE REST #
	############

	# a do-while in python:
	if f != pt_max:
		continue
	else:
		break 	
```

### Tracking Affine Transformations ###

We next regard &nbsp;<b>`step 5`</b>&nbsp; and consider how the dataset must be
rotated between iterations to ensure that the minimum nonnegative slope 
condition is still valid. The code here will be inserted at the point 
marked &nbsp;`### THE REST ###`&nbsp; in the above snippet

</section>
