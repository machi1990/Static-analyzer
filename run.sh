#!/bin/bash
find tests/*.c -exec ./analyzer.byte -interval {} -unroll 3 \;
