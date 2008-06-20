function Action=randomAgent_start(intObservations, doubleObservations)
   
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
        intArray(intIndex) = rand()*max + min;
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
    
    intArray = int32(intArray);
    Action{1} = intArray;
    Action{2} = doubleArray;
end


