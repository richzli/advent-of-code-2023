import sys, os

EXTENSION = ".hs"

day, part = map(int, sys.argv[1:3])
overwrite = True
if len(sys.argv) > 3:
    overwrite = False
fifth = (day-1) // 5
folder = f"{fifth*5+1:02}-{(fifth+1)*5:02}"

os.system(f"runhs {folder}/{day:02}-{part}{EXTENSION} < {folder}/{day:02}.in")
if part == 1 and overwrite:
    open(f"{folder}/{day:02}-2{EXTENSION}", "w").write(open(f"{folder}/{day:02}-1{EXTENSION}", "r").read())