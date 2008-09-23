function returnMessage=test_message_agent_message(theMessageJavaObject)
    theMessage=char(theMessageJavaObject);

	if isempty(theMessage)
		returnMessage='empty';
		return
	end

	if strcmp(theMessage,'null')
		returnMessage='';
		return
	end

	if strcmp(theMessage,'empty')
		returnMessage='';
		return
	end
	
	returnMessage=sprintf('%s',theMessage);
		
    
% // 
% //  if(inMessage==null)
% //      return "null";
% // 
% // if(inMessage.equals(""))
% //     return "empty";
% //  
% //  if(inMessage.equals("null"))
% //      return null;
% // 
% //  if(inMessage.equals("empty"))
% //      return "";
% // 
% //  return new String(inMessage);

end