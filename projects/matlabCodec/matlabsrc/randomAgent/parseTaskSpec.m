function  taskSpecMatlabObject=parseTaskSpec(taskSpec)
%split into v:e:o:a:r
[matchstr splitstr] = regexp(taskSpec, ':', 'match', 'split');
%version number
version = splitstr(1);
%episodic or continuous
episodic = splitstr(2);
%break the observation string on '_'
[mat observation] = regexp(splitstr(3), '_', 'match', 'split');
%remove all [ and ]
observation = regexprep(observation{1}, '\[', '');
observation = regexprep(observation(:), '\]', '');
%remove ',' from the types array ex: [f,f] should now be ff
observation(2) = regexprep(observation(2), ',', '');
%store the ranges in cell arrays to make accessing values easier
for i=3:1:length(observation)
    [mat observation{i}] = regexp(observation{i}, ',' , 'match', 'split');
end
%break the action string on '_'
[mat action] = regexp(splitstr(4), '_', 'match', 'split');
action = regexprep(action{1}, '\[', '');
action = regexprep(action(:), '\]', '');
action(2) = regexprep(action(2), ',', '');
for i=3:1:length(action)
    [mat action{i}] = regexp(action{i}, ',' , 'match', 'split');
end
%reward should be a range, but havent built this in yet as its not always
%defined
reward = splitstr(5);

taskSpecMatlabObject.version=version;
taskSpecMatlabObject.episodic=episodic;
taskSpecMatlabObject.observation=observation;
taskSpecMatlabObject.action=action;
taskSpecMatlabObject.reward=reward;
