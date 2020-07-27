# What now?

## Reporting

* The confidence regions are not rectangles in the HxC plane. We will have to report the four (x,y) corner coordinates that define them.
* Compute the regions at the 90%, 95%, 99%, and 99.9%
* Is there a way to provide the inverse result, i.e., given a point in HxC, return the level of the smallest confidence region to which it belongs?
* Provide a routine for, given a point and the length of the sequence, return the result of the test.

## Experiments
* Select a sequence **z** that produces an "emblematic" point in HxC, e.g., the median point.
* Study "trajectories" of the point by transforming the sequence:
    * Determine the largest exponent k such that the f^-k sequence produced from **z** is still Uniform
    * Same for other transformations

