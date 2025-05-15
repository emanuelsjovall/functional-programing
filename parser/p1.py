n = 1024
b = 2
m = 1             # m := 1
s = 0             # s := 0
p = 1             # p := 1

while n:
    q = n // b              # q := n / b (integer division)
    r = n - q * b           # r := n - q * b (manual mod)
    print(r)                # write r
    s = p * r + s           # s := p * r + s
    p = p * 10              # p := p * 10
    n = q                   # n := q

print(s)  # write s