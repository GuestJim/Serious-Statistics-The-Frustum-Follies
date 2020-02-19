import os, sys, math

os.system("TITLE 3DMark Build")

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"

refPath		=	scriptPath + "3DMark TS Bench.3dmdef"

def	listSTR	(LIST):
	return	str(LIST).replace("[", "").replace("]", "").replace("'", "")

print("The generated folders should be placed in the 3DMark program folder with \"SSFF\" sub-folder")
print("\n\n")

listDPS			=	input("Samples per Square Edge (eg. 120 for 1080): ")
listDPS			=	[int(j)	for j in listDPS.replace(" ", "").split(",")]

listRATIO	=	["16x9", "04x3", "21x9", "9x16"]
listRATIO	=	input("Aspect Ratios (Default: " + listSTR(listRATIO) + "): ") or listRATIO
try:
	listRATIO	=	listRATIO.replace(" ", "").split(",")
except:
	pass
#	this is to handle passing it using the list, so it will not try to apply the string functions to it.

LOOP			=	int(input("Number of test loops (default 3): ")) or 3

def	ratDIM	(TYPE):
	return(	[	int(TYPE.split("x")[0]),
				int(TYPE.split("x")[1])		]
		)

def	RES	(ratWIDTH, ratHEIGHT, dps):
	con			=	math.sqrt((16*9)/(ratWIDTH*ratHEIGHT))
	outWIDTH	=	round(con	*	ratWIDTH	*	dps	/	2)	*	2
	outHEIGHT	=	round(con	*	ratHEIGHT	*	dps	/	2)	*	2
	return([outWIDTH,	outHEIGHT,	con])

def	customBENCH	(RATIO, dps):
	ratWIDTH	=	ratDIM(RATIO)[0]
	ratHEIGHT	=	ratDIM(RATIO)[1]
	
	outWIDTH	=	RES(ratWIDTH, ratHEIGHT, dps)[0]
	outHEIGHT	=	RES(ratWIDTH, ratHEIGHT, dps)[1]
	con			=	RES(ratWIDTH, ratHEIGHT, dps)[2]
	
	TYPE	=	str(16*int(dps)) + "x" + str(9*int(dps))
	
	FILE	=	scriptPath + TYPE + "A\\" + TYPE + "A-" + RATIO + "-TS.3dmdef"

	if not os.path.exists(scriptPath + TYPE + "A\\"):
		os.mkdir(scriptPath + TYPE + "A\\")

	if not os.path.exists(FILE):
		with open(refPath, 'r') as fref, open(FILE, 'w') as fout:
			for line in fref:
				fout.write(line
					.replace("!WIDTH!",		str(outWIDTH))		\
					.replace("!HEIGHT!",	str(outHEIGHT))
				)
			fout.close()

def	customRES (dps, listRAT = listRATIO):
	TYPE	=	str(16*int(dps)) + "x" + str(9*int(dps))
	FILE	=	scriptPath + TYPE + "A\\" + TYPE + "A Resolutions.csv"
	
	if not os.path.exists(FILE):
		with open(FILE, 'w') as fout:
			fout.write("Ratio,Width,Height\n")
			for	ratio in listRAT:
				ratWIDTH	=	ratDIM(ratio)[0]
				ratHEIGHT	=	ratDIM(ratio)[1]
				
				outWIDTH	=	RES(ratWIDTH, ratHEIGHT, dps)[0]
				outHEIGHT	=	RES(ratWIDTH, ratHEIGHT, dps)[1]
				
				fout.write(
					ratio + "," +
					str(outWIDTH) + "," + 
					str(outHEIGHT) + "\n"
				)					
			fout.close()
			
for DPS in	listDPS:
	for	ratio in	listRATIO:
		customBENCH(ratio, dps = DPS)
	customRES(DPS, listRATIO)

def	benches	():
	listBENCH	=	""
	for dps in listDPS:
		TYPE		=	str(16*int(dps)) + "x" + str(9*int(dps)) + "A"
		listBENCH	=	listBENCH + "loop3DM(\"" + TYPE + "\")\n"
	return(listBENCH)

if not os.path.exists(scriptPath + "3DMark Script.py"):
	with open(scriptPath + "3DMark Script - Reference.py", 'r') as fref, open(scriptPath + "3DMark Script.py", 'w') as fout:
		for line in fref:
			fout.write(line
				.replace("!LOOP!",		str(LOOP))	\
				.replace("!listRATIO!",	str(listRATIO))	\
				.replace("!BENCH!",		benches())
				)
		fout.close

# os.system("pause")