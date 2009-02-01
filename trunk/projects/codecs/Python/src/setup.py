#! /usr/bin/env python

from distutils.core import setup

import rlglue

setup(name="RL_Glue_PythonCodec",
      version='2.0',
      description="RL-Glue Python Codec",
      author="Brian Tanner",
      author_email=" brian@tannerpages.com",
      url="http://glue.rl-community.org/Home/Extensions/python-codec",
      packages=['rlglue','rlglue.agent','rlglue.environment','rlglue.network','rlglue.utils'],
	  license='Apache License Version 2.0',
     )

