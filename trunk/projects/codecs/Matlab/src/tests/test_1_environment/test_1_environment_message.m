function returnMessage=test_1_environment_message(theMessageJavaObject)
	global test_1_environment_struct;
    theMessage=char(theMessageJavaObject);
    
    timesToPrint=mod(test_1_environment_struct.stepCount,3);
    
    returnMessage=sprintf('%s|',theMessage);
    %Start at one because the top end will be inclusive unlike C for loop
    for i=1:1:timesToPrint
        returnMessage=sprintf('%s%d.',returnMessage,test_1_environment_struct.stepCount);
    end
    returnMessage=sprintf('%s|%s',returnMessage,theMessage);
end