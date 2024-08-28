import subprocess
import os
import importlib.util
import glob

spec = importlib.util.spec_from_file_location("assembler", "BatPU-2/assembler.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

files = glob.glob("./test/**/*.mc")
for file in files:
    os.remove(file)

cases = os.listdir("./test/base")
cases = [case for case in cases if case.endswith(".as")]
passed = 0
for i, case in enumerate(cases):
    try:
        print(f"Running test {i + 1}/{len(cases)}: {case}")
        module.assemble(f"./test/base/{case}", f"./test/base/{case}.mc")
        with open(f"./test/base/{case}.mc", "r") as f:
            expected = f.readlines()
        # run the cargo app
        subprocess.run(["cargo", "run", "--", f"./test/base/{case}", f"./test/base/{case}.tmp.mc"])
        with open(f"./test/base/{case}.tmp.mc", "r") as f:
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
    except Exception as e:
        print(f"Exception occured during test: {e}")

print(f"Passed {passed}/{len(cases)} tests")
