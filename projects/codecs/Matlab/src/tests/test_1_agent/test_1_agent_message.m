function returnMessage=test_1_agent_message(theMessageJavaObject)
  
    global test_1_agent_struct;
    if(isempty(test_1_agent_struct))
        test_1_agent_struct.stepCount=0;
    end
    if(isempty(test_1_agent_struct.stepCount))
        test_1_agent_struct.stepCount=0;
    end
    
    theMessage=char(theMessageJavaObject);
    
    timesToPrint=mod(test_1_agent_struct.stepCount,3);
    
    returnMessage=sprintf('%s|',theMessage);
    %Start at one because the top end will be inclusive unlike C for loop
    for i=1:1:timesToPrint
        returnMessage=sprintf('%s%d.',returnMessage,test_1_agent_struct.stepCount);
    end
    returnMessage=sprintf('%s|%s',returnMessage,theMessage);

%         int timesToPrint = stepCount % 3;
%         StringBuffer b = new StringBuffer();
% 
%         b.append(inMessage);
%         b.append("|");
%         for (int i = 0; i < timesToPrint; i++) {
%             b.append(stepCount);
%             b.append(".");
%         }
%         b.append("|");
%         b.append(inMessage);
%         return b.toString();
end