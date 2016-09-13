#!/bin/bash
java -cp src SquareFree $@
cd bee20160531
./BumbleBEE models/f4.bee -dimacs out.dim out.map
cd ..
