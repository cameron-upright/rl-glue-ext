function taskspec=mountainCar_init()
global theState minPos maxPos minVel maxVel goalPos;
minPos = -1.2;
maxPos = 0.6;
minVel = -0.07;
maxVel = 0.07;
goalPos = 0.5;
theState = double([]);
taskspec='2:e:2_[f,f]_[-1.2,0.6]_[-0.07,0.07]:1_[i]_[0,2]:[-1,0]';
