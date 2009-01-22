#! /usr/bin/env python

from distutils.core import setup

import rlglue

setup(name="RL_Glue_PythonCodec",
      version=rlglue.version,
      description="RL-Glue Python Codec 2.0",
      author="Brian Tanner",
      author_email=" brian@tannerpages.com",
      url="http://glue.rl-community.org/Home/Extensions/python-codec",
      packages=['rlglue','rlglue.agent','rlglue.environment','rlglue.network','rlglue.utils'],
     )

