import * as R from 'ramda'
import fs from 'fs/promises'

const input = await fs.readFile('input/01', 'utf-8')

const sums = R.compose(
  R.reverse,
  R.sort((a, b) => a - b),
  R.map(R.compose(R.sum, R.map(Number), R.split("\n"))),
  R.split("\n\n")
)

console.log(sums(input).at(0))
console.log(R.sum(R.take(3, sums(input))))
