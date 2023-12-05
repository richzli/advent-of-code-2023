import sys, os
import subprocess
from markdownify import markdownify
from bs4 import BeautifulSoup

YEAR = "2023"
EXTENSION = ".hs"
template = open("Advode.hs", "r").read()

day = int(sys.argv[1])
fifth = (day-1) // 5
folder = f"{fifth*5+1:02}-{(fifth+1)*5:02}"
cookie = open("session").read()

os.makedirs(folder, exist_ok=True)
os.system(f"curl -A \"github.com/richzli/advent-of-code-{YEAR} via curl\" --cookie \"session={cookie}\" https://adventofcode.com/{YEAR}/day/{day}/input > {folder}/{day:02}.in")
open(f"{folder}/{day:02}.md", "w").write(
    markdownify(
        str(BeautifulSoup(
            str(subprocess.check_output(
                ["curl", "-A", "\"github.com/richzli/advent-of-code-{YEAR} via curl\"", "--cookie", f"\"session={cookie}\"", f"https://adventofcode.com/{YEAR}/day/{day}"]
            ))
        , "html.parser").find("article", attrs={"class": "day-desc"})).encode("utf-8").decode("unicode-escape")
    )
)
open(f"{folder}/{day:02}-1{EXTENSION}", "w").write(template)