function taskspec=mountainCar_init()
global mountainCar__internalState;
minPos = -1.2;
maxPos = 0.6;
minVel = -0.07;
maxVel = 0.07;
goalPos = 0.5;
theState = double([]);

mountainCar__internalState.minPos=minPos;
mountainCar__internalState.maxPos=maxPos;
mountainCar__internalState.minVel=minVel;
mountainCar__internalState.maxVel=maxVel;
mountainCar__internalState.goalPos=goalPos;
mountainCar__internalState.theState=theState;

taskspec='2:e:2_[f,f]_[-1.2,0.6]_[-0.07,0.07]:1_[i]_[0,2]:[-1,0]';
