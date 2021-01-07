let inp = [0,6,1,7,2,19,20];
let N = 3e7;
let m = {};
let v = inp.pop();
inp.forEach((n, i) => m[n] = 1 + i);
for (let n = 1 + inp.length; n < N; n++) {
  let i = m[v] || n;
  m[v] = n;
  v = n - i;
}
console.log(v);
