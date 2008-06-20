function stringResponse=mountainCar_message(theMessage)
if nargin < 1
    theMessage = 'null message';
end
    fprintf(1,'mountainCar_message called with message %s\n',theMessage);
    stringResponse='No response to message';