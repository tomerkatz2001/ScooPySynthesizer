#!/usr/bin/python
import json
import sys
from ast import parse
from ast2parse import ast2parse

print(json.dumps(ast2parse(parse(sys.argv[1])), indent=4))

