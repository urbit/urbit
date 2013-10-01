#!/usr/bin/python

import re

names = {
         "<"  : "gal",
         ")"  : "per",
         "|"  : "bar",
         ">"  : "gar",
         "["  : "sel",
         "\\" : "bas",
         "#"  : "hax",
         ";"  : "sem",
         "$"  : "buc",
         "-"  : "hep",
         "]"  : "ser",
         "_"  : "cab",
         "{"  : "kel",
         "~"  : "sig",
         "%"  : "cen",
         "}"  : "ker",
         "'"  : "soq",
         ":"  : "col",
         "^"  : "ket",
         "*"  : "tar",
         ","  : "com",
         "+"  : "lus",
         "`"  : "tec",
         "\"" : "doq",
         "&"  : "pam",
         "="  : "tis",
         "."  : "dot",
         "@"  : "pat",
         "?"  : "wut",
         "/"  : "fas",
         "("  : "pel",
         "!"  : "zap"}

runef = open("runelist.txt")
digraphs = []
phonemictexts = {}
symbols = {}
for line in runef:
	if len(line) < 3:
		continue
	digraph = line.strip()
	phonemictext = digraph
	for graph,text in names.iteritems():
		phonemictext = phonemictext.replace(graph,text)
	digraphs.append(digraph)
	phonemictexts[digraph] = phonemictext
	phonemictexts[phonemictexts[digraph]] = digraph
	symbols[digraph] = "%" + phonemictext[0] + phonemictext[2:4] + phonemictext[5]
	symbols[symbols[digraph]] = digraph

#for i,j,k in zip(digraphs,phonemictexts,symbols):
#	print i,k,j

hoonf = open("../../urb/zod/arvo/hoon.hoon")
genemode = False
genes = {}
for line in hoonf:
	if not genemode:
		if line[0:8] == "++  gene":
			genemode = True
		continue
	if line[0:2] == "++":
		genemode = False
		continue
	m = re.match(r'.*\[(%....) (.*)\].*',line)
	if not m:
		continue
	if not m.group(1) in symbols:
		continue
	genes[symbols[m.group(1)]] = m.group(0)
	genes[genes[symbols[m.group(1)]]] = m.group(1)

for i in digraphs:
	print i, symbols[i], phonemictexts[i]
	if i in genes:
		print genes[i]
