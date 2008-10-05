/* 
* Copyright (C) 2008, Brian Tanner

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */
#include <stdio.h>

#include "TheGame.h"


int gameState=10;

void new_game(){
	gameState=10;
}

void play_one_step(int whichAction){
	if(whichAction==0)
		gameState--;

	if(whichAction==1)
		 gameState++;
}

void game_startup(){
		printf("\"The Game\" starting up...\n");
		printf("...intializing fun factors\n");
		printf("...calibrating goof exponents\n");
		printf("etc.\n");
	}



int main(int argc, char** argv) {
	int theConnection=0;
	game_startup();

	theConnection=setup_rlglue_network();
	runEnvironmentEventLoop(theConnection);

	printf("Thanks for playing the game!\n");
	teardown_rlglue_network(theConnection);
	
	return 1;
}


	
