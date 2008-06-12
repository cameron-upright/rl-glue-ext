mountainCar.init='mountainCar_init';
mountainCar.start='mountainCar_start';
mountainCar.step='mountainCar_step';
mountainCar.message='mountainCar_message';
mountainCar.cleanup='mountainCar_cleanup';
mountainCar.freeze='mountainCar_freeze';
mountainCar.setstate='mountainCar_setstate';
mountainCar.getstate='mountainCar_getstate';
mountainCar.setrandomseed='mountainCar_setrandomseed';
mountainCar.getrandomseed='mountainCar_getrandomseed';
%Create a mapping of functions to names, to be passed into Java. This is
%so java knows the name of the functions to call for each of the
%rl-glue functions.
global version episodic observation action reward;

theEnvCell=envCellFromStruct(mountainCar);

theJavaEnv=org.rlcommunity.rlglue.ext.codecs.matlab.MatlabEnvironmentCodec(theEnvCell);