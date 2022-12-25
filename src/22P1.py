import re

raw_grid, sequence = open(0).read().split('\n\n')

W = max([len(x) for x in raw_grid.splitlines()])

grid = [line + ' ' * (W - len(line)) for line in raw_grid.splitlines()]

r, c, dr = [0, 0, 0]
dc = 1

while grid[r][c] != '.':
    c += 1

for x, y in re.findall(r'(\d+)([RL]?)', sequence):
    x = int(x)
    for _ in range(x):
        nr = r
        nc = c
        while True:
            nr = (nr + dr) % len(grid)
            nc = (nc + dc) % len(grid[0])
            if grid[nr][nc] != ' ':
                break
        if grid[nr][nc] == '#':
            break
        r = nr
        c = nc
    if y == 'R':
        dr, dc = dc, -dr
    elif y == 'L':
        dr, dc = -dc, dr

if dr == 0:
    if dc == 1:
        k = 0
    else:
        k = 2
else:
    if dr == 1:
        k = 1
    else:
        k = 3

print(1000 * (r + 1) + 4 * (c + 1) + k)
