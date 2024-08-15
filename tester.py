import subprocess
import os
import importlib.util

spec = importlib.util.spec_from_file_location("assembler", "BatPU-2/assembler.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

cases = os.listdir("./test")
cases = [case for case in cases if case.endswith(".as")]
passed = 0
for i, case in enumerate(cases):
    print(f"Running test {i + 1}/{len(cases)}: {case}")
    module.assemble(f"./test/{case}", f"./test/{case}.mc")
    with open(f"./test/{case}.mc", "r") as f:
        expected = f.readlines()
    # run the cargo app
    subprocess.run(["cargo", "run", "--", f"./test/{case}", f"./test/{case}.tmp.mc"])
    with open(f"./test/{case}.tmp.mc", "r") as f:
        actual = f.readlines()
    if expected == actual:
        passed += 1
        print(f"Test {case} passed")
    else:
        print(f"Test {case} failed")
        for i in range(min(len(expected), len(actual))):
            if expected[i] != actual[i]:
                print(f"Expected: {expected[i]}")
                print(f"Actual: {actual[i]}")
                break

print(f"Passed {passed}/{len(cases)} tests")