killall rl_glue
sleep 1
rl_glue &
sleep 1
../codecs/C/tests/test_1_environment &
sleep 1
java -Xmx128M -classpath ../codecs/Java/dist/JavaCodec.jar org.rlcommunity.rlglue.tests.Test_1_Agent &
sleep 1
../codecs/C/tests/.libs/test_rl_episode_experiment