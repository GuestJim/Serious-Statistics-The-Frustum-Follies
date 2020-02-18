import sys, os

#	pull the results from the XML files into a CSV using a similar method to the OCAT - Modular - PA.py script to go through all the available folders. This will allow it to work with any number of loops, and any other system

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"

resultsPath	=	scriptPath + "Results\\"

def	dataSplit	(FILE):
	TYPE	=	FILE.split(" - ")[0].replace("A", "")
	RUN		=	FILE.split(" - ")[1].split("\\")[0]
	RATIO	=	FILE.rsplit("-", 3)[2].replace("04x3", "4x3").replace("x", ":")
	return	[TYPE,	RUN	,	RATIO]

def	resultsFPS	(FILE):
	DATA	=	[]
	with(open(FILE, 'r')) as xmlIN:
		for line in xmlIN:
			if "GraphicsTest" in line:
				DATA.append(line.split(">")[1].split("</")[0])
	return(DATA)

def	listclean	(list):
	return	str(list).replace("[", "").replace("]", "").replace("\'", "") + "\n";

def	listbreak	(list):
	return	str(list).replace("[", "").replace("]", "").replace("\'", "\"").replace(", ", ",\n")

listfile	=	[]

for paths, folders, files in os.walk(resultsPath):
	for file in files:
		if file.endswith(".xml"):
			# print(paths.replace(resultsPath, "") + "\\" + file)
			listfile.append(paths.replace(resultsPath, "") + "\\" + file)

os.chdir(scriptPath)
with open("3DMark Results.csv", 'w') as fout:
	fout.write("Type, Run, Ratio, Test1, Test2\n")
	for	file in listfile:
		fout.write(listclean(dataSplit(file) + resultsFPS(resultsPath + file)))
	fout.close()


listTYPE	=	[]

for paths, folders, files in os.walk(scriptPath):
	for file in files:
		if file.endswith("Resolutions.csv"):
			if [int(file.split("x")[0]), file] not in listTYPE:
				listTYPE.append([int(file.split("x")[0]), file])
listTYPE.sort()
listTYPE	=	[i.pop(-1) for i in listTYPE]

TYPEs	=	[i.split(" ")[0].replace("A", "") for i in listTYPE]

listRES	=	""
listRAT	=	[]

for file in listTYPE:
	listRAT.append("x" + file.split("x")[1].split("A ")[0])
	listRES	=	listRES + "x" + file.split("x")[1].split("A ")[0] + "\t=\tdimNAME(t(data.frame(\n"
	with open(scriptPath + file.split(" ")[0] + "\\" + file) as RES:
		for line in RES:
			if "Ratio,Width,Height" not in line:
				listRES	=	listRES + "\tc(" + line.replace("\n", "").split(",", 1)[1] + "),\n"
		RES.close()
	listRES	=	listRES + ")))\n\n"

listRES	=	listRES.replace("),\n)", ")\n)")

listRatio	=	[]
for file in listfile:
	cRATIO	=	file.split("\\")[1].split("-")[1].replace("x", ":")
	if cRATIO[0] == "0":
		cRATIO	=	cRATIO[1:]
	#		to remove zero padding from 04:3, for example (I like consistent character counts, but sometimes want rid of them)
	if cRATIO not in listRatio:
		listRatio.append(cRATIO)
listRatio.remove("16:9")
listRatio.insert(0, "16:9")
#	to make sure 16:9 is at the front of the list


DIMS	=	""
AREAS	=	""

for	ratio in listRAT:
	DIMS	=	DIMS + "\t\"" + ratio + "\"\t=\t" + ratio + ",\n"
	AREAS	=	AREAS + "\tDIMS$" + ratio + "[, \"Width\"]\t*\tDIMS$" + ratio + "[, \"Height\"],\n"

DIMS	=	DIMS[:-2]
AREAS	=	AREAS[:-2]

if not os.path.exists(scriptPath + "3DMark Results.r"):
	with open(scriptPath + "3DMark Results - Reference.r", 'r') as fref, open(scriptPath + "3DMark Results.r", 'w') as fout:
		for line in fref:
			fout.write(line
				.replace("!PATH!",		scriptPath.replace("\\", "/"))	\
				.replace("!TYPE!",		listbreak(TYPEs))				\
				.replace("!listRatio!",	listbreak(listRatio))
				.replace("!RES!",		listRES)						\
				.replace("!DIMS!",		DIMS)							\
				.replace("!AREAS!",		AREAS)
			)
		fout.close()