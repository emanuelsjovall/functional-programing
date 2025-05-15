k = 3
n = 16
m = 1
output = []

while n - m:
    if m - (m // k) * k:
        pass  # skip
    else:
        # note an exponentiation below
        output.append(m ** (2))
    m = m + 1  # an inline comment

# yet another comment, this time inside a statement!
# var = int(input())

print(output)