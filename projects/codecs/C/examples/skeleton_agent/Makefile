LDFLAGS := $(LDFLAGS)
CFLAGS := $(CFLAGS) -Wall -pedantic

all: SkeletonAgent

SkeletonAgent: SkeletonAgent.o 
	$(CC)  $(LDFLAGS) SkeletonAgent.o -lrlutils -lrlagent -lrlgluenetdev -o SkeletonAgent      

SkeletonAgent.o: SkeletonAgent.c
	$(CC)  $(CFLAGS) -c SkeletonAgent.c -o SkeletonAgent.o 

clean:
	rm -rf SkeletonAgent SkeletonAgent.exe *.o









