const fs = require('fs');
const readline = require('readline');

class StreamVals {
    constructor() {
        this.vals = [];
    }

    current() {
        return this.prev(0);
    }

    prev(step) {
        return this.vals[this.vals.length - (1 + step)]
    }

    currentIndex() {
        return this.vals.length - 1;
    }

    push(val) {
        this.vals.push(parseInt(val));
    }
}

class ReadStream {
    constructor(filename, intrfc) {
        this.filename = filename;
        this.callbacks = [];
        this.intrfc = intrfc;
    }

    register(f, acc) {
        this.callbacks.push([f, acc]);
    }

    start() {
        const rl = readline.createInterface({
            input: fs.createReadStream(this.filename),
            output: process.stdout,
            terminal: false
        });

        rl.on('line', line => {
            this.intrfc.push(line);

            for (let i = 0; i < this.callbacks.length; ++i) {
                const callback = this.callbacks[i];
                this.callbacks[i][1] = callback[0](this.intrfc, callback[1]);
            }
        });

        rl.on('close', () => {
            this.callbacks.forEach(([_, v]) => {
                console.log(v);
            });
        });
    }
}

function sum(a,b,c) { return a + b + c }

const rs = new ReadStream("day01/input.txt", new StreamVals());
rs.register((i, acc) => {
    if (i.prev(1) < i.current())
        return ++acc

    return acc;
}, 0);
rs.register((i, acc) => {
    if (i.currentIndex < 3)
        return acc;

    if (sum(i.prev(3), i.prev(2), i.prev(1)) < sum(i.prev(2), i.prev(1), i.current()))
        return ++acc;

    return acc;
}, 0);
rs.start();
