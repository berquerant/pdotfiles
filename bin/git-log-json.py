import sys
import subprocess
import json

DATETIME_FORMAT = '%Y-%m-%d %H:%M:%S'
FORMAT_MAP = {
    "hash": "%H",
    "tree": "%T",
    "parents": "%P",
    "author_name": "%an",
    "author_email": "%ae",
    "author_date": "%ad",
    "author_date_relative": "%ar",
    "committer_name": "%cn",
    "committer_email": "%ce",
    "committer_date": "%cd",
    "committer_date_relative": "%cr",
    "subject": "%s",
    "body": "%b",
}
format_sep = "|||"
format_kv_sep = "__=__"
format_arg = format_sep.join(sorted(
    k + format_kv_sep + v for k, v in FORMAT_MAP.items()
))
format_quote_right = "'''___"
cmd = [
    "git",
    "log",
    f"--pretty=format:{format_arg}{format_quote_right}",
    f"--date=format:{DATETIME_FORMAT}",
    *sys.argv[1:],
]
p = subprocess.run(cmd, check=True, text=True, stdout=subprocess.PIPE, stderr=None)
result = p.stdout.rstrip().removesuffix(format_quote_right)
for line in result.split(format_quote_right):
    line = line.lstrip()
    d = {}
    for x in line.split(format_sep):
        k, v = x.split(format_kv_sep, 1)
        d[k] = v
    d["parents"] = d["parents"].split()
    print(json.dumps(d, separators=(',', ':')))
