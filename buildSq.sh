#!/bin/bash
java -cp src SquareFree $@
cd bee20160830
./BumbleBEE models/f4.bee -dimacs out.dim out.map
cd ..
