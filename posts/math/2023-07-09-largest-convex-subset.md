---
title: Largest Convex Subset
link-citations: true
tableOfContents: true
tags: Geometry, Convexity
---

# [Introduction]{.underline} # 

Consider a closed, nonconvex figure in the plane. What is its largest convex 
subset? This does seem like a well founded question, and the problem as a 
whole can be used as a model for exploration as I'll discuss.

:::{#fig1}

![Largest convex subset](/images/doro.jpg){ width=40% }

:::


[<b>Figure 1</b>](#fig1) illustrates the question at hand. The amoeba shape is 
the nonconvex set in question, and the shaded triangle is a guess at the 
largest convex subset. Don't look to closely though, it's easy to pick out ways 
in which the triangular estimation can be improved. For instance, the bottom and 
left sides should be flush with the amoeba, and the right side of the triangle 
should also be pushed further until it's flush ([<b>Figure 2</b>](#fig2)).

:::{#fig2}

![Improved largest convex subset](/images/doro_annotated.png){ width=40% }

:::

Of course, we also need to consider tradeoffs of curvature. Would the northwest
side of the triangle capture more area if it curved outwards like one half of 
an oval? We'd better formalize the problem.

# [Formulation]{.underline} #

We're optimizing over a collection, a set of sets. If we call the nonconvex 
set $S$, the collection of interest here is
$$C=\{\ T\subset\mathbb{R}^2 \mid \mathcal{[}T\subset S\mathcal{]}\ ;\ \mathcal{[}\lambda x+(1-\lambda) y\in T\ \ \forall\ \ \lambda\in [0, 1],\ (x,y)\in T\mathcal{]}\ \}$$
The expression is a lot to parse, so I've written the two constraints with
$\mathcal{[}$calligraphic brackets$\mathcal{]}$ 
around them, and with a semicolon separating them. The first constraint is that
 $T$ must be a subset of $S$, and the second is that $S$ is convex. Qualitatively 
though, $C$ is just the set of all subsets of $S$ that are convex.

:::{.indent}

## The Optimization Problem ##

What we're missing, in order to have an well-formulated optimization problem,
is an objective. How can we express the area of a potential maximal convex
subset?

:::



