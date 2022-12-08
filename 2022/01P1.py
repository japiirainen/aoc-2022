#!/usr/bin/env python3
input = [[int(line) for line in g.splitlines()] for g in open('input/01').read().split('\n\n')]
max_group = max([sum(g) for g in input])
assert 72478 == max_group
print(max_group)