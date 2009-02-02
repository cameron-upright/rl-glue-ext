fileName="Matlab.wiki.template"
outfileName="Matlab.wiki"

subs={}
subs['MATLAB-USER-DEV-VERSION']='2.0-RC-Final';
subs['MATLAB-USER-DEV-LINK']='http://rl-glue-ext.googlecode.com/files/Matlab-Codec-1.0-RC3.tar.gz';

f = file(fileName)
newlines = []
for line in f:
	for key,value in subs.iteritems():
		if key in line:
			line=line.replace(key,value)
	newlines.append(line)

outfile = file(outfileName, 'w')
outfile.writelines(newlines)