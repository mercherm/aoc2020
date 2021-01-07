class Ring {
  constructor(cups) {
    this.hash = {};
    let prev = this.cursor = new Cup(cups[0]);
    this.hash[cups[0]] = prev;
    let min = Infinity, max = -Infinity;
    for (let i = 1; i < cups.length; i++) {
      let v = cups[i];
      if (v < min) min = v;
      if (v > max) max = v;

      let cur = new Cup(v, prev);
      this.hash[v] = cur;
      prev.right = cur;
      prev = cur; 
    }
    this.min = Math.min(cups[0], min);
    this.max = Math.max(cups[0], max);
    this.cursor.left = prev;
    prev.right = this.cursor;
  }

  find(v) {
    return this.hash[v];
  }

  move() {
    let first = this.cursor.right;
    let second = first.right;
    let third = second.right;

    this.cursor.right = third.right;
    this.cursor.right.left = this.cursor;
    
    let values = [first.value, second.value, third.value];
    let v = this.cursor.value;

    for (;;) {
      if (--v < 1) v = this.max;
      if (!values.includes(v)) break;
    }

    let dest = this.find(v);
    let destR = dest.right;

    dest.right = first;
    first.left = dest;
    
    destR.left = third;
    third.right = destR;

    this.cursor = this.cursor.right;
  }

  toString() {
    let o = [this.cursor.value];
    let cur = this.cursor.right;
    while (cur !== this.cursor) {
      o.push(cur.value);
      cur = cur.right;
    }
    return o.join('');
  }
}

class Cup {
  constructor(value, left, right) {
    this.value = value;
    this.left = left;
    this.right = right;
  }
}

let inp = "952438716".split('').map(Number);
let p2 = Array.from({length:1e6}, (_,i)=> i < 9 ? inp[i] : 1 + i);
let r = new Ring(p2);
for (let i = 1e7; i--;) r.move();
let one = r.find(1);
let fst = one.right.value;
let snd = one.right.right.value;
console.log(fst, snd, fst * snd);
