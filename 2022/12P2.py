from collections import deque

g = [list(x) for x in open(0).read().splitlines()]

for r, row in enumerate(g):
    for c, col in enumerate(row):
        if g[r][c] == 'S':
            g[r][c] = 'a'
        if g[r][c] == 'E':
            er, ec = r, c
            g[r][c] = 'z'

Q = deque()
Q.append((0, er, ec))
V = {(er, ec)}

while Q:
    dist, cr, cc = Q.popleft()
    for nr, nc in [(cr+1, cc), (cr-1, cc), (cr, cc+1), (cr, cc-1)]:
        if (nr, nc) in V:
            continue
        if nr < 0 or nc < 0 or nr >= len(g) or nc >= len(g[0]):
            continue
        if ord(g[cr][cc]) - ord(g[nr][nc]) > 1:
            continue
        if g[nr][nc] == 'a':
            print(dist+1)
            exit()
        V.add((nr, nc))
        Q.append((dist+1, nr, nc))
