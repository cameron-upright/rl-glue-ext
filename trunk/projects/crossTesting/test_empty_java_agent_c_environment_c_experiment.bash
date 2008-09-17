killall rl_glue
sleep 1
rl_glue &
sleep 1
../codecs/C/tests/test_empty_environment &
sleep 1
java -Xmx128M -classpath ../codecs/Java/dist/JavaCodec.jar org.rlcommunity.rlglue.RLGlueCore --agent org.rlcommunity.rlglue.tests.Test_Empty_Agent &
sleep 1
../codecs/C/tests/.libs/test_empty_experiment