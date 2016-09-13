#!/bin/bash
cd src
java Main $@
cd ../bee20160531
./BumbleBEE models/f4.bee -dimacs out.dim out.map
cd ..
