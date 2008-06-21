function theEnv=createMatlabMountainCar()

theEnv.env_init='mountainCar_init';
theEnv.env_start='mountainCar_start';
theEnv.env_step='mountainCar_step';
theEnv.env_message='mountainCar_message';
theEnv.env_cleanup='mountainCar_cleanup';
theEnv.env_set_state='mountainCar_setstate';
theEnv.env_get_state='mountainCar_getstate';
theEnv.env_set_random_seed='mountainCar_setrandomseed';
theEnv.env_get_random_seed='mountainCar_getrandomseed';

end