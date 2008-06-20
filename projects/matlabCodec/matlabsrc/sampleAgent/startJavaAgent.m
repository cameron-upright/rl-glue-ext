function theJavaThread=startJavaAgent(theJavaAgent)
    global theJavaThread;
    theJavaThread=org.rlcommunity.rlglue.ext.codecs.matlab.JavaLoaderWrapper(theJavaAgent);
end