#!/usr/bin/env python3

C = 1
INSTS = []

for inst in open('input/10').read().splitlines():
    if inst == 'noop':
        INSTS.append(C)
    else:
        x = inst.split()[1]
        INSTS += [C, C]
        C += int(x)

print(sum([x*y+y for x, y in list(enumerate(INSTS))[19::40]]))
