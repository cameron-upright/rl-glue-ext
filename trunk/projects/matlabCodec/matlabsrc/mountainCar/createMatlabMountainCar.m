function theEnv=createMatlabMountainCar()

theEnv.env_init='mountainCar_init';
theEnv.env_start='mountainCar_start';
theEnv.env_step='mountainCar_step';
theEnv.env_message='mountainCar_message';
theEnv.env_cleanup='mountainCar_cleanup';
theEnv.env_freeze='mountainCar_freeze';
theEnv.env_setstate='mountainCar_setstate';
theEnv.env_getstate='mountainCar_getstate';
theEnv.env_setrandomseed='mountainCar_setrandomseed';
theEnv.env_getrandomseed='mountainCar_getrandomseed';

end