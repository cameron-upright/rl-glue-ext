LDFLAGS := $(LDFLAGS)
CFLAGS := $(CFLAGS) -Wall -pedantic

all: SkeletonExperiment

SkeletonExperiment: SkeletonExperiment.o 
	$(CC)  $(LDFLAGS) SkeletonExperiment.o -lrlutils -lrlexperiment -lrlgluenetdev -o SkeletonExperiment      

SkeletonExperiment.o: SkeletonExperiment.c
	$(CC)  $(CFLAGS) -c SkeletonExperiment.c -o SkeletonExperiment.o 

clean:
	rm -rf SkeletonExperiment SkeletonExperiment.exe *.o









