function RL_env_message(theMessage)
global rlglue__struct;

        public synchronized String RL_env_message(String theString){
                return E.env_message(theString);
        }
        public synchronized String RL_agent_message(String theString){
                return A.agent_message(theString);
        }

        public synchronized void RL_init() {
                A.agent_init(E.env_init());
                totalEpisodes=0;
        }


