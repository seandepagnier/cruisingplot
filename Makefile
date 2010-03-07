SOURCES = algebra.scm cruisingplot.scm computation.scm draw.scm frequency.scm glshortcuts.scm sensor.scm gps.scm history.scm net.scm plot.scm sound.scm spherical.scm units.scm weather.scm wind.scm

OBJECTS = $(SOURCES:.scm=.o)

all: cruisingplot

%.o : %.scm
	csc -c $<

cruisingplot: $(OBJECTS)
	csc -o crusingplot $(OBJECTS)


clean:
	rm -f cruisingplot
