grid = [(list(map(int, line)))
        for line in open("input/08").read().splitlines()]


def is_visible(r, c, k):
    return all(grid[r][a] < k for a in range(c)
               ) or all(grid[r][a] < k for a in range(c + 1, len(grid[r]))
                        ) or all(grid[a][c] < k for a in range(r)
                                 ) or all(grid[a][c] < k for a in range(r + 1, len(grid)))


v = 0
for r in range(len(grid)):
    for c in range(len(grid[r])):
        if is_visible(r, c, grid[r][c]):
            v += 1

print(v)
