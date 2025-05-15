a = 4
b = 4
s = 3
output = []

while a:
    c = a ** s
    d = 2 ** a
    output.append(c)
    output.append(d)
    a = a - 1

output.append(a)
print(output)