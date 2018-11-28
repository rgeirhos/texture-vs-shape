#!/usr/bin/env python

#script to calculate visual degrees of experiment.
#copied from: http://osdoc.cogsci.nl/miscellaneous/visual-angle/

from math import atan2, degrees

h = 30.2 # Monitor height in cm
d = 100 # Distance between monitor and participant in cm
r = 1200 # Vertical resolution of the monitor
size_in_px = 256 # The stimulus size in pixels

# Calculate the number of degrees that correspond to a single pixel. This will
# generally be a very small value, something like 0.03.
deg_per_px = degrees(atan2(.5*h, d)) / (.5*r)
print '%s degrees correspond to a single pixel' % deg_per_px
# Calculate the size of the stimulus in degrees
size_in_deg = size_in_px * deg_per_px
print 'The size of the stimulus is %s pixels and %s visual degrees' \
	% (size_in_px, size_in_deg)

