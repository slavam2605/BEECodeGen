#!/bin/bash
cd src
java Main $@
cd ../bee20160830
./BumbleBEE models/f4.bee -dimacs out.dim out.map
cd ..
