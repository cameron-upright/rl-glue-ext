function taskspec=mountainCar_init()
global theState minPos maxPos minVel maxVel goalPos;
minPos = -1.2;
maxPos = 0.6;
minVel = -0.7;
maxVel = 0.7;
goalPos = 0.5;
theState = [];
taskspec='2:e:2_[f,f]_[-2.3800000000000003,1.78]_[-1.084,0.944]:1_[i]_[0,2]:[-1,0]';
fprintf(1,'mountainCar_message called with message %s\n',taskspec);