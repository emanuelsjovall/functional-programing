k = 3
n = 16
m = 1                    # \m := 1;\
while n - m:             # \while n - m do\
    if m - m / k * k: # \if m - m/k*k then\
        pass             # \  skip;\
    else:                # \else\
        print(m)         # \  write m;\
    m = m + 1            # \m := m + 1;\