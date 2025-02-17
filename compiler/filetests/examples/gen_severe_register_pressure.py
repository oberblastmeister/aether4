n = 4000
res = ""
res += "int main() {\n"
res += "int x1 = 1234;\n"
res += "int x2 = 12;\n"
for i in range(3, n + 1):
    res += f"int x{i} = x{i - 1} + x{i - 2};\n"
res += "int res = x1;\n"
for i in range(1, n + 1):
    res += f"res += x{i};\n"
res += "return res;\n"
res += "}\n"
print(res)
