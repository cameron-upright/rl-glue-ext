LDFLAGS := $(LDFLAGS)
CFLAGS := $(CFLAGS) -Wall -pedantic

all: SkeletonEnvironment

SkeletonEnvironment: SkeletonEnvironment.o 
	$(CC)  $(LDFLAGS) SkeletonEnvironment.o -lrlutils -lrlenvironment -lrlgluenetdev -o SkeletonEnvironment      

SkeletonEnvironment.o: SkeletonEnvironment.c
	$(CC)  $(CFLAGS) -c SkeletonEnvironment.c -o SkeletonEnvironment.o 

clean:
	rm -rf SkeletonEnvironment SkeletonEnvironment.exe *.o









