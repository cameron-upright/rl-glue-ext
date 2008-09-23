function returnMessage=test_message_environment_message(theMessageJavaObject)
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
end