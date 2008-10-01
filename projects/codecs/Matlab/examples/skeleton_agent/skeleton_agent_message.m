function returnMessage=skeleton_agent_message(theMessageJavaObject)
%Java strings are objects, and we want a Matlab string
    inMessage=char(theMessageJavaObject);

	if strcmp(inMessage,'what is your name?')==1
		returnMessage='my name is skeleton_agent, Java edition!';
    else
		returnMessage='I don\''t know how to respond to your message';
end