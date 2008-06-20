function Action=sampleAgent_start(intObservations, doubleObservations)
    whos intObservations
    whos doubleObservations
    
    fprintf(1,'sampleAgent_start called \n');
    Action{1}=[0];
    Action{2}=[];
end