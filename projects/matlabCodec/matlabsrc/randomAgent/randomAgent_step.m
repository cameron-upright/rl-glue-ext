function Action=randomAgent_step(Reward,theObservation)
   
%   Action=chooseEpsilonGreedy(intObservations,doubleObservations);
global TaskSpecMatlabObject;

%ActionSpec contains the action information from the taskSpec
actionSpec=TaskSpecMatlabObject.action;

numActionDimensions=str2double(actionSpec{1}(1));
doubleIndex = 1;
intIndex=1;
intArray = int32([]);
doubleArray = [];

for i=1:numActionDimensions
    if(i > numActionDimensions)
        return;
    end
    if(actionSpec{2}(i) == 'i')
        %i wonder if this will work, random[0,1] * max + min
        max = cell2mat(actionSpec{i+2}(2));
        min = cell2mat(actionSpec{i+2}(1));
        max = str2double(max);
        min = str2double(min);
       intArray(intIndex) = floor(rand()*(max+1)) + min;

        intIndex = intIndex + 1;
    else
        %i wonder if this will work, random[0,1] * max + min
        max = cell2mat(actionSpec{i+2}(2));
        min = cell2mat(actionSpec{i+2}(1));
        max = str2double(max);
        min = str2double(min);
        doubleArray(doubleIndex) = rand()*max + min;
        doubleIndex = doubleIndex + 1;
    end
    
    %Cheat to do well in mountain car for testing
%    if(theObservation.doubleArray(2)<0)
%        intArray(1)=0;
%    else
%        intArray(1)=2;
%    end
    Action.intArray=int32(intArray);
    Action.doubleArray=doubleArray;
end