function theJavaThread=startMountainCar(theJavaEnv)
    global theJavaThread;
    theJavaThread=org.rlcommunity.rlglue.ext.codecs.matlab.JavaLoaderWrapper(theJavaEnv);
end