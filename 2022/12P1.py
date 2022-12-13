from collections import deque

g = [list(x) for x in open(0).read().splitlines()]

for r, row in enumerate(g):
    for c, col in enumerate(row):
        if g[r][c] == 'S':
            sr, sc = r, c
            g[r][c] = 'a'
        if g[r][c] == 'E':
            er, ec = r, c
            g[r][c] = 'z'

Q = deque()
Q.append((0, sr, sc))
V = {(sr, sc)}

while Q:
    dist, cr, cc = Q.popleft()
    for nr, nc in [(cr+1, cc), (cr-1, cc), (cr, cc+1), (cr, cc-1)]:
        if (nr, nc) in V:
            continue
        if nr < 0 or nc < 0 or nr >= len(g) or nc >= len(g[0]):
            continue
        if ord(g[nr][nc]) - ord(g[cr][cc]) > 1:
            continue
        if nr == er and nc == ec:
            print(dist+1)
            exit()
        V.add((nr, nc))
        Q.append((dist+1, nr, nc))
