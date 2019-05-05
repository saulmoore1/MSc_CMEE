#!/usr/bin/bash

# Shell script to run the Lotka-Volterra model scripts and profile them, printing the results to screen

python -m cProfile LV1.py
python -m cProfile LV2.py 1 0.5 1 0.5 30.
python -m cProfile LV3.py
python -m cProfile LV4.py
python -m cProfile LV5.py
