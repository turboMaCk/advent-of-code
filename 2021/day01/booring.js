fs = require('fs');

const content = fs.readFileSync("day01/input.txt", {encoding:'utf8', flag:'r'});
const vals = content.split('\n').map(str => parseInt(str));

function calcIncreases1(vals) {
    return vals.reduce((acc, val, index) => {
        if (index < 1) return acc;
        if (vals[index-1] < val) return ++acc;
        return acc;
    }, 0)
}

function calcIncreases2(vals) {
    let sum = (i) => vals[i] + vals[i-1] + vals[i-2];
    return vals.reduce((acc, val, index) => {
        if (index < 3) return acc;
        if (sum(index-1) < sum(index)) return ++acc;
        return acc;
    }, 0)
}

console.log(calcIncreases1(vals));
console.log(calcIncreases2(vals));
