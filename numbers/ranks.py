from __future__ import annotations
from sage.all import *
import sys

def getRank(p: int, bc: list[int]) -> int:
    curve = EllipticCurve(bc)
    return curve.conductor()

def getConductor(p: int, bc: list[int]) -> int:
    curve = EllipticCurve(bc)
    return curve.rank_bound()

def getNumbers(numbers: list[list[str]]) -> list[str|list[int]]:
    result = []
    for line in numbers:
        numeric = all([x.strip('-').isnumeric() for x in line])
        if numeric:
            result.append([int(x) for x in line])
        else:
            result.append("N/A")
    return result

if __name__ == "__main__":
    with open(sys.argv[1], "r") as dataFile:
        with open("conductorsAndRanks.csv", "w") as outputFile:
            numsStrings = [line.split(",") for line in dataFile.read().splitlines()][1:]
            relevantNumStrings = [ [line[1], line[4], line[5]] for line in numsStrings ]#[:5000]
            nums = getNumbers(relevantNumStrings)
            outputFile.write("conductor,rank")
            for i, pbc in enumerate(nums):
                if pbc != "N/A":
                    rank = getRank(pbc[0], pbc[1:])
                    conductor = getConductor(pbc[0], pbc[1:])
                    outputFile.write(f"\n{rank},{conductor}")
                    outputFile.flush()
                    print(str(i) + " " + str(rank) + ", " + str(conductor))
                else:
                    outputFile.write(f"\nN/A,N/A")
                    outputFile.flush()
                    print(print(str(i) + " " + pbc))