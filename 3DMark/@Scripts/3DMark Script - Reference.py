import os, sys

LOOP	=	!LOOP!

listRATIO	=	!listRATIO!

# OUT	=	"C:\\Users\\packe\\Desktop\\Open Share\\SSFF\\Results"
OUT	=	sys.argv[0].rsplit("\\", 1)[0] + "\\Results"
if not os.path.exists(OUT):
	os.mkdir(OUT)

def BENCH	(TYPE, NAME, COUNT = "",  output=OUT):
	fullNAME	=	TYPE + "-" + NAME
	RESULTS		=	output + "\\" + TYPE + " - " + COUNT + "\\" + fullNAME
	if not os.path.exists(output + "\\" + TYPE + " - " + COUNT + "\\"):
		os.mkdir(output + "\\" + TYPE + " - " + COUNT + "\\")
	os.system("3DMarkCmd.exe --definition=SSFF\\" + TYPE + "\\" + fullNAME + ".3dmdef --audio=off --online=off --out=\"" + RESULTS + ".3dmark-result\" --export=\"" + RESULTS + ".xml\"")
	# print("3DMarkCmd.exe --definition=SSFF\\" + TYPE + "\\" + fullNAME + ".3dmdef --audio=off --online=off --out=\"" + RESULTS + ".3dmark-result\" --export=\"" + RESULTS + ".xml\"")

def	loop3DM	(TYPE):
	for count in range(1, LOOP + 1):
		count	=	str(count)
		for	RATIO in listRATIO:
			BENCH(TYPE, RATIO + "-TS", count)

#Time Spy
!BENCH!

# os.system("pause")