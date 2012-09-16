# Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
#
# This Program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version. 

SOURCES = algebra.scm cruisingplot.scm computation.scm draw.scm frequency.scm sensor.scm gps.scm magnetometer.scm history.scm net.scm plot.scm sound.scm spherical.scm units.scm weather.scm wind.scm wifiaimer.scm utilities.scm ahrs.scm options.scm relay.scm tiltcompensation.scm matrix.scm infix2prefix.scm leastsquares.scm autopilot.scm types.scm filter.scm task.scm config.scm motor.scm vector.scm quaternion.scm

CSOURCES = geomag70/geomag70.c libgps.c

CFLAGS = -g
LDFLAGS = -L/usr/local/lib -lgps

OBJECTS = $(SOURCES:.scm=.o) $(CSOURCES:.c=.o)

all: cruisingplot

%.o : %.scm
	csc -c $<

cruisingplot: $(OBJECTS)
	csc -o cruisingplot $(OBJECTS) $(LDFLAGS)


clean:
	rm -f cruisingplot $(OBJECTS)
