const fs = require('fs');

const file = fs.readFileSync('./input.txt', { encoding: "utf-8"})

const calculate = ([l, w, h]) => 2 * l * w + 

const result = file.split('x').map(x => parseInt(x, 10)).map(calculate).reduce((acc, prev) => acc + prev, 0)


console.log('resut', result)