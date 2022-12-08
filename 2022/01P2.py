#!/usr/bin/env python3
input = [[int(line) for line in g.splitlines()] for g in open('input/01').read().split('\n\n')]
sums = list(reversed(sorted(sum(x) for x in input)))
res = sum(sums[:3])
assert res == 210367
print(res)