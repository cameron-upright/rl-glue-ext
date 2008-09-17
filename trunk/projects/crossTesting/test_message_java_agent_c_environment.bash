killall rl_glue
sleep 1
rl_glue &
sleep 1
../codecs/C/tests/test_message_environment &
sleep 1
java -Xmx128M -classpath ../codecs/Java/dist/JavaCodec.jar org.rlcommunity.rlglue.RLGlueCore --agent org.rlcommunity.rlglue.tests.Test_Message_Agent &
sleep 1
../codecs/C/tests/.libs/test_message_experiment