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

for i in range(0, len(INSTS), 40):
    for j in range(40):
        print(end='#' if abs(INSTS[i + j] - j) <= 1 else ' ')
    print()
