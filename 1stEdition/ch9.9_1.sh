#!/bin/bash

echo 123$'\x7f'456 | ./ch9.9_1

echo 123$'\x7f' | ./ch9.9_1

echo $'\x7f'123 | ./ch9.9_1
