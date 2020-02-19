library(readr)
library(ggplot2)
library(tidyr)

gameGAQF	=	"SSFF"

RATIO	=	c(
!listRatio!
)

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"
gWIDTH		=	21
gHEIGH		=	9

ratioCOLOR	=	list(
	"16:9"	=	"#ff0000",
	"4:3"	=	"#0000ff",
	"5:4"	=	"#00ffff",
	"16:10"	=	"#ff7f7f",
	"21:9"	=	"#00ff00",
	"9:16"	=	"#ffff00"
	)
#	update this list if you are using different ratios

ratioAREAS				=	(16/9)/c(16/9, 4/3, 5/4, 16/10, 21/9, 9/16)
names(ratioAREAS)		=	RATIO
ratioAREAS["21:9"]		=	ratioAREAS["21:9"]^-1
ratioAREAS				=	as.data.frame(ratioAREAS)
ratioAREAS$Ratio		=	RATIO
colnames(ratioAREAS)	=	c("Area", "Ratio")
#	because it is along a different axis the 9:16 area must be inverted

areaCOLOR	=	c(
	"1"					=	"#ff0000",
	"1.11111111111111"	=	"#ff7f7f",
	"1.3125"			=	"#00ff00",
	"1.33333333333333"	=	"#0000ff",
	"1.42222222222222"	=	"#00ffff",
	"3.16049382716049"	=	"#ffff00"
	)

areaLABELS	=	c(
	"16:9"	=	1,
	"4:3"	=	4/3,
	"5:4"	=	64/45,
	"16:10"	=	10/9,
	"21:9"	=	1.3125,
	"9:16"	=	3.160494
	)
#	update these lists if you are using different ratios

if (interactive())	{
	setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics FF/3DMark")
}	else	{
	pdf(NULL)
}

resultsFull	=	read_csv("3DMark Results.csv")

results	=	resultsFull

TYPE	=	c(
!TYPE!
)

results$Type	=	factor(results$Type,	levels = TYPE,	ordered = TRUE)
results$Run		=	factor(results$Run,						ordered = TRUE)
results$Ratio	=	factor(results$Ratio,	levels = RATIO,	ordered = TRUE)


dimNAME	=	function(DATA)	{
	rownames(DATA)	=	RATIO
	colnames(DATA)	=	c("Width", "Height")
	return(DATA)
}

!RES!

DIMS	=	list(
!DIMS!
)

PIXELS	=	rbind(
!PIXELS!
)
rownames(PIXELS)	=	TYPE
PIXELS			=	as.data.frame(PIXELS)
PIXELS$Type		=	TYPE

results	=	merge(ratioAREAS, results, by = "Ratio")

longPIXELS	=	pivot_longer(PIXELS,
					cols		=	RATIO,
					names_to	=	"Ratio",
					values_to	=	"Pixels"
					)

results	=	merge(longPIXELS, results, by = c("Ratio", "Type"))


labelBreakF	=	function(breaks)	paste0(rep(c("", "\n"), length.out = length(breaks)), breaks)
rem_		=	function(INPUT)	gsub("_", " ", INPUT)
sci2norm	=	function(DATA)	format(DATA, scientific = FALSE)
ms2FPS		=	function(DATA, r = 0)	round(1000/DATA, r)
FPS2ms		=	function(DATA, r = 2)	round(1000/DATA, r)
round2		=	function(DATA, r = 2)	round(DATA, r)


roundMEAN	=	function(DATA, r = 2)	{
	return(round(mean(DATA), r))
}

meanGEO	=	function(DATA)	{
	out			=	exp(mean(log(DATA)))
	names(out)	=	"ms"
	return(out)
}

StErr		=	function(DATA)	sd(DATA) / sqrt(length(DATA))
#	Standard Error function

customSave	=	function(type="", device=ggdevice, plot = last_plot(), width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	if	(device	==	"png"	|	device == "both")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".png"), plot = plot, device="png", width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf"	|	device == "both")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".pdf"), plot = plot, device="pdf", width=width, height=height)
	}
}

GROUPS	=	list(	Ratio	=	results$Ratio,
					Type	=	results$Type,
					Area	=	results$Area,
					Pixels	=	results$Pixels
					)
DATAS	=	list(Time_Spy_GFX_Test_1 = results$Test1,	Time_Spy_GFX_Test_2 = results$Test2)

dataMEAN	=	aggregate(DATAS, GROUPS, roundMEAN)
dataERROR	=	aggregate(DATAS, GROUPS, StErr)

dataGEO		=	aggregate(DATAS, list(Ratio = results$Ratio), meanGEO)
#	may want to use a different approach, because of how weighted the results may be to one or the other area


dataALL	=	merge(dataMEAN,	dataERROR,
				by = c("Type", "Ratio", "Area", "Pixels"),
				sort = FALSE,
				suffixes = c("_Mean", "_Error")
			)

longMEAN	=	pivot_longer(dataMEAN,
					cols			=	grep("Test", colnames(dataMEAN)),
					names_to		=	"Test",
					names_ptypes	=	list(Test = factor(ordered = TRUE)),
					values_to		=	"Mean"
				)

longERROR	=	pivot_longer(dataERROR,
					cols			=	grep("Test", colnames(dataERROR)),
					names_to		=	"Test",
					names_ptypes	=	list(Test = factor(ordered = TRUE)),
					values_to		=	"Error"
				)

longALL	=	merge(longMEAN,	longERROR,	by = c("Type", "Ratio", "Area", "Pixels", "Test"),	sort = FALSE)

levels(longALL$Test)	=	rem_(longALL$Test)

dataALL$Type	=	factor(dataALL$Type,	levels = TYPE,	ordered = TRUE)
dataALL$Ratio	=	factor(dataALL$Ratio,	levels = RATIO,	ordered = TRUE)

longALL$Type	=	factor(longALL$Type,	levels = TYPE,	ordered = TRUE)
longALL$Ratio	=	factor(longALL$Ratio,	levels = RATIO,	ordered = TRUE)


library(tableHTML)
OCCHTML	=	function(DATA, rNAMES	=	FALSE)	{
	tableHTML(DATA, rownames = rNAMES, class="OCC") %>%
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE) %>%
	replace_html('_', ' ', replace_all = TRUE)
}

writeOCC	=	function(DATA, dataNAME, name, ...)	{
	write_tableHTML(OCCHTML(DATA, ...), file = paste0(name, " - ", dataNAME,".html"))
}

htmlALL			=	dataALL
htmlALL$Area	=	signif(htmlALL$Area, 6)
htmlALL[, grep("Error", colnames(htmlALL))]	=	signif(htmlALL[, grep("Error", colnames(htmlALL))], 2)

writeOCC(htmlALL,									"dataALL",	"SSFF")
writeOCC(PIXELS[, -grep("Type", colnames(PIXELS))],	"PIXELS",	"SSFF",	rNAMES = TRUE)

for(place in 1:length(DIMS))	{
	writeOCC(as.data.frame(DIMS[place]),	paste0("Dimensions - ", names(DIMS)[place]),	"SSFF",	rNAMES = TRUE)
}


DODGE	=	position_dodge(width = 1)
#	necessary for proper alignment of error bars, and that this be applied for both the bars and the coloumns

backRECT	=	function(DATA)	{
	if (is.data.frame(DATA))	lenDATA	=	nrow(DATA)
	if (is.factor(DATA))		lenDATA	=	length(DATA)
	
	PATTERN	=	1:(length(TYPE) / 2) * 2
	FACET	=	unique(DATA$Test)
	
	xmin	=	rep(PATTERN	- 0.5,	length.out = lenDATA)
	xmax	=	rep(PATTERN + 0.5,	length.out = lenDATA)
	
	xmin	=	PATTERN	- 0.5
	xmax	=	PATTERN + 0.5
	ymin	=	-Inf
	ymax	=	Inf
	
	out	=	as.data.frame(cbind(xmin, xmax, ymin, ymax))
	
	out	=	merge(FACET, out)
	colnames(out)[1]	=	"Test"
	
	return(out[rep(seq_len(nrow(out)), lenDATA/nrow(out)),])
	#	this will repeat the frame by creating a sequence of ROW indices that repeat the necessary number of times
}

backRECT.log	=	function(DATA)	{
	if (is.data.frame(DATA))	lenDATA	=	nrow(DATA)
	if (is.factor(DATA))		lenDATA	=	length(DATA)
	
	PATTERN	=	1:(length(TYPE) / 2) * 2
	FACET	=	unique(DATA$Test)
	
	xmin	=	rep(PATTERN	- 0.5,	length.out = lenDATA)
	xmax	=	rep(PATTERN + 0.5,	length.out = lenDATA)
	
	xmin	=	PATTERN	- 0.5
	xmax	=	PATTERN + 0.5
	ymin	=	1
	ymax	=	Inf
	
	out	=	as.data.frame(cbind(xmin, xmax, ymin, ymax))
	
	out	=	merge(FACET, out)
	colnames(out)[1]	=	"Test"
	
	return(out[rep(seq_len(nrow(out)), lenDATA/nrow(out)),])
	#	this will repeat the frame by creating a sequence of ROW indices that repeat the necessary number of times
}

backLINES	=	function(types = TYPE)	{
	LINES	=	""
	
	for (i in 1:(length(types) - 1))	{
		LINES	=	paste0(LINES, "geom_vline(aes(xintercept = ", i + 0.5, "), color = \"black\", size = 1.0, alpha = 0.75), ")
	}
	LINES	=	paste0("list(", substr(LINES, 1, nchar(LINES)-2), ")")
	return(eval(parse(text = LINES)))
}


if (identical(names(ratioCOLOR), RATIO)){
	scaleCOLORS	=	list(
		scale_fill_manual(	values = ratioCOLOR,	name = "Aspect Ratio"),
		scale_color_manual(	values = ratioCOLOR,	name = "Aspect Ratio") 
	)
}	else	{
	scaleCOLORS	=	list(
		scale_color_hue(),
		scale_fill_hue()
	)
}
#	backup for if the list for manual colors is not set

typeFIND	=	function(DATA, TYPE, COL = "Type")	{
	rows	=	c()
	for (i in TYPE){
		rows	=	union(rows, grep(i, DATA[, COL]))
	}
	return(rows)
}
#	this is necessary for the some graphs as a means to isolate certain results




#	FPS by Ratio and Resolution
ggplot(data = longALL, aes(x = Type, y = Mean, group = Ratio, fill = Ratio)) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - Frame Rate (FPS)\nGreater is better") + labs(caption = "RTX 2060") +
geom_rect(aes(
	xmin	=	backRECT(longALL)$xmin,
	xmax	=	backRECT(longALL)$xmax,
	ymin	=	backRECT(longALL)$ymin,
	ymax	=	backRECT(longALL)$ymax),
	fill	=	"darkgrey",	alpha	=	0.05) + 
geom_col(position = DODGE) + 
geom_errorbar(aes(ymin = Mean - Error,	ymax = Mean + Error), 
	position	=	DODGE,
	width		=	0.5,
	color		=	"magenta") + 
backLINES() + 
facet_grid(cols = vars(Test)) + 
scaleCOLORS + 
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank()) + 
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) + 
scale_y_continuous(
	name		=	"Mean Frame Rate (FPS, 15 runs)",
	breaks		=	0:6*30,
	limits		=	c(0, 180),
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Time (ms, 15 runs)",
		labels	=	FPS2ms
		)
) + 
scale_x_discrete(
	name			=	"Appoximate Pixel Area",
	labels			=	labelBreakF,
	expand			=	c(0.02, 0.5)
)

customSave("3DMark Time Spy - FPS by Res")


#	Pixels per Frame per Millisecond by Ratio and Resolution
ggplot(data = longALL, aes(x = Type, y = (1000/Mean)/Pixels, group = Ratio, fill = Ratio)) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - ms per Pixel per Frame\nLower is better") + labs(caption = "RTX 2060") +
geom_rect(aes(
	xmin	=	backRECT(longALL)$xmin,
	xmax	=	backRECT(longALL)$xmax,
	ymin	=	backRECT(longALL)$ymin,
	ymax	=	backRECT(longALL)$ymax),
	fill	=	"darkgrey",	alpha	=	0.05) + 
geom_col(position = DODGE) + 
backLINES() + 
facet_grid(cols = vars(Test)) + 
scaleCOLORS + 
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank()) + 
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) + 
scale_y_continuous(
	name		=	"Mean Time per Pixel per Frame (ms / Pixel / Frame, 15 runs)",
	breaks		=	0:3*1e-5, 1,
	labels		=	sci2norm,
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		labels	=	paste0)
) + 
scale_x_discrete(
	name			=	"Appoximate Pixel Area",
	labels			=	labelBreakF,
	expand			=	c(0.02, 0.5)
)

customSave("3DMark Time Spy - ms per Pixels COL")


#	Pixels per Frame per Millisecond by Ratio and Resolution
ggplot(data = longALL, aes(x = Pixels, y = (1000/Mean)/Pixels, group = Ratio, color = Ratio, fill = Ratio)) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - ms per Pixel per Frame\nLower is better") + labs(caption = "RTX 2060") +
geom_smooth(data = longALL[-grep("7680x4320", longALL$Type), ], se = FALSE) + 
geom_point(shape = 21, color = "black") +  
facet_grid(cols = vars(Test)) + 
scaleCOLORS + 
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank(),
	panel.spacing		=	unit(2, "lines")) + 
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) + 
scale_y_continuous(
	name		=	"Mean Time per Pixel per Frame (ms / Pixel / Frame, 15 runs)",
	breaks		=	0:3*1e-5, 1,
	labels		=	sci2norm,
	expand		=	c(0.02, 0),
	# trans		=	"log10",
	sec.axis	=	dup_axis(
		labels	=	paste0)
) + 
scale_x_log10(
	name			=	"Pixel Area",
	breaks			=	PIXELS[, "16:9"],
	labels			=	labelBreakF(TYPE),
	expand			=	c(0.02, 0)
)

customSave("3DMark Time Spy - ms per Pixels")


#	Pixels per Frame per Millisecond by Ratio and Resolution with LOG scales and regression
ggplot(data = longALL, aes(x = Pixels, y = (1000/Mean)/Pixels, group = Ratio, color = Ratio, fill = Ratio)) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - ms per Pixel per Frame\nLower is better") + labs(caption = "RTX 2060") +
geom_smooth(data = longALL[-grep("7680x4320", longALL$Type), ], se = FALSE) + 
geom_point(shape = 21, color = "black") +  
facet_grid(cols = vars(Test)) + 
scaleCOLORS + 
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank(),
	panel.spacing		=	unit(2, "lines")) + 
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) + 
scale_y_continuous(
	name		=	"Mean Time per Pixel per Frame (ms / Pixel / Frame, 15 runs)",
	breaks		=	0:3*1e-5, 1,
	labels		=	sci2norm,
	expand		=	c(0.02, 0),
	trans		=	"log10",
	sec.axis	=	dup_axis(
		labels	=	paste0)
) + 
scale_x_log10(
	name			=	"Pixel Area",
	breaks			=	PIXELS[, "16:9"],
	labels			=	labelBreakF(TYPE),
	expand			=	c(0.02, 0)
)

customSave("3DMark Time Spy - ms per Pixels log")


#	Frame Time by Area
ggplot(data = longALL, aes(x = Area, y = 1000/Mean, group = Type, color = Type, fill = as.factor(Area))) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - Frame Time (ms) by Area\nLower is better") + labs(caption = "RTX 2060") +
geom_line() +
geom_point(shape = 21, color = "black") + 
scale_color_hue(name = "Resolution") + 
scale_fill_manual(
	name	=	"Aspect Ratio",
	values	=	areaCOLOR,
	breaks	=	areaLABELS,
	labels	=	names(areaLABELS),
	)+
facet_grid(cols	=	vars(Test)) +
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank(),
	panel.spacing		=	unit(1.5, "lines"),
	# axis.text.x			=	element_text(angle = -45, hjust = 0)
	) +
guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time (ms, 15 runs)",
	breaks		=	1000/c(1, 2, 5, 10, seq(0, 170, by = 15)),
	labels		=	round2,
	expand		=	c(0.02, 0.01),
	trans		=	"log10",
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 15 runs)",
		labels	=	ms2FPS)
) +
expand_limits(y = 1000) + 
scale_x_continuous(
	name	=	"Area Relative to 16:9",
	breaks	=	ratioAREAS$Area,
	labels	=	labelBreakF(ratioAREAS$Ratio),
	expand	=	c(0.02, 0)
)

customSave("3DMark Time Spy - ms by Area", width = 16, height = 12)


#	Frame Time by Area Zoomed via Facets
ggplot(data = longALL, aes(x = Area, y = 1000/Mean, group = Type, color = Type, fill = as.factor(Area))) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - Frame Time (ms) by Area\nLower is better") + labs(caption = "RTX 2060") +
geom_line() +
geom_point(shape = 21, color = "black") + 
scale_color_hue(name = "Resolution") + 
scale_fill_manual(
	name	=	"Aspect Ratio",
	values	=	areaCOLOR,
	breaks	=	areaLABELS,
	labels	=	names(areaLABELS),
	)+
facet_grid(cols	=	vars(Test), rows = vars(Type), as.table = FALSE, scales = "free_y", switch = "y") +
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank(),
	panel.spacing		=	unit(1.5, "lines"),
	# axis.text.x			=	element_text(angle = -45, hjust = 0)
	) +
guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time (ms, 15 runs)",
	breaks		=	1000/c(1, 2, 5, 10, seq(0, 170, by = 15)),
	labels		=	round2,
	expand		=	c(0.02, 0.01),
	trans		=	"log10",
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 15 runs)",
		labels	=	ms2FPS)
) +
scale_x_continuous(
	name	=	"Area Relative to 16:9",
	breaks	=	ratioAREAS$Area,
	labels	=	labelBreakF(ratioAREAS$Ratio),
	expand	=	c(0.02, 0)
)

customSave("3DMark Time Spy - ms by Area Zoom", width = 16, height = 21)


#	Frame Time by Area with Discrete X, but not using as it removes that useful information of relative area difference
ggplot(data = longALL, aes(x = as.factor(Area), y = 1000/Mean, group = Type, color = Type, fill = as.factor(Area))) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - Frame Time (ms) by Area\nLower is better") + labs(caption = "RTX 2060") +
geom_line() +
geom_point(shape = 21, color = "black") + 
scale_color_hue(name = "Resolution") + 
scale_fill_manual(
	name	=	"Aspect Ratio",
	values	=	areaCOLOR,
	breaks	=	areaLABELS,
	labels	=	names(areaLABELS),
	)+
facet_grid(cols	=	vars(Test)) +
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank(),
	panel.spacing		=	unit(1.5, "lines"),
	# axis.text.x			=	element_text(angle = -45, hjust = 0)
	) +
guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time (ms, 15 runs)",
	breaks		=	1000/c(1, 2, 5, 10, seq(0, 170, by = 15)),
	labels		=	round2,
	expand		=	c(0.02, 0.01),
	trans		=	"log10",
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 15 runs)",
		labels	=	ms2FPS)
) +
expand_limits(y = 1000) + 
scale_x_discrete(
	name	=	"Area Relative to 16:9",
	breaks	=	ratioAREAS$Area,
	labels	=	paste0(round(ratioAREAS$Area, 3), "\n(", ratioAREAS$Ratio, ")"),
	expand	=	c(0.02, 0)
)


#	Frame Time by Ratio and Resolution
#		a Log scale is used because of the huge difference at 8K
ggplot(data = longALL, aes(x = Type, y = 1000/Mean, group = Ratio, fill = Ratio)) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - Frame Time (Log Scale)\nLess is better") + labs(caption = "RTX 2060") +
geom_rect(aes(
	xmin	=	backRECT.log(longALL)$xmin,
	xmax	=	backRECT.log(longALL)$xmax,
	ymin	=	backRECT.log(longALL)$ymin,
	ymax	=	backRECT.log(longALL)$ymax),
	fill	=	"darkgrey",	alpha	=	0.05) + 
geom_col(position = DODGE) + 
backLINES() + 
facet_grid(cols = vars(Test)) + 
scaleCOLORS + 
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank()) + 
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) + 
scale_y_log10(
	name		=	"Mean Frame Time (ms, 15 runs)",
	breaks		=	1000/c(1, 1:6*30),
	labels		=	round2,
	limits		=	c(1, 1000),
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 15 runs)",
		labels	=	ms2FPS
		# labels		=	1000/sort(c(ytimes, 1000/5:1))
		)
) + 
scale_x_discrete(
	name			=	"Appoximate Pixel Area",
	labels			=	labelBreakF,
	expand			=	c(0.02, 0.5)
)


#Frame Rate by Area (breaks and labels are the Resolutions)
#	log scale necessary for area (and appropriate) but do not feel this is a necessary graph
ggplot(data = longALL, aes(x = Pixels, y = Mean, group = Ratio, fill = Ratio)) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - Frame Rate (FPS)\nGreater is better") + labs(caption = "RTX 2060") +
geom_point(shape = 21, color = "black") + 
facet_grid(rows = vars(Test), switch = "y") + 
scaleCOLORS + 
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank()) + 
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) + 
scale_y_continuous(
	name		=	"Mean Frame Rate (FPS, 15 runs)",
	breaks		=	0:6*30,
	# breaks		=	c(0, round(yrates, 2)),
	limits		=	c(0, 180),
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis()
) + 
scale_x_log10(
	name			=	"Appoximate Pixel Area",
	breaks			=	PIXELS[, "16:9"],
	labels			=	labelBreakF(gsub("A", "", TYPE)),
	expand			=	c(0.02, 0)
)


#Frame Time by Area (breaks and labels are the Resolutions)
#	log scale necessary for area (and appropriate)
GFX1low		=	c("640x360", "736x414", "960x540")
GFX1high	=	c("1280x720", "1600x900", "1920x1080", "2560x1440", "3200x1800", "3840x2160", "5120x2880", "6400x3600")
GFX2low		=	c("640x360", "736x414", "960x540")
GFX2high	=	c("1280x720", "1600x900", "1920x1080", "2560x1440", "3200x1800", "3840x2160", "5120x2880", "6400x3600")
#	groups for what appears to be CPU-bound and GPU-bound

ggplot(data = longALL, aes(x = Pixels, y = 1000/Mean, group = Ratio, fill = Ratio)) + 
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "3DMark Time Spy - Frame Time (ms)\nLess is better (Logarithmic Scales)") + labs(caption = "RTX 2060") +
geom_smooth(aes(color = Ratio),	se = FALSE,	method = "lm",
	data = 	longALL[intersect(	grep("Test 1", longALL$Test),
								typeFIND(longALL,	GFX1low)), ]	) + 
geom_smooth(aes(color = Ratio),	se = FALSE,	method = "lm",
	data = 	longALL[intersect(	grep("Test 1", longALL$Test), 
								typeFIND(longALL,	GFX1high)), ]	) + 								
geom_smooth(aes(color = Ratio),	se = FALSE,	method = "lm",
	data = 	longALL[intersect(	grep("Test 2", longALL$Test), 
								typeFIND(longALL,	GFX2low)), ]	) + 
geom_smooth(aes(color = Ratio),	se = FALSE,	method = "lm",
	data = 	longALL[intersect(	grep("Test 2", longALL$Test), 
								typeFIND(longALL,	GFX2high)), ]	) + 
geom_point(shape = 21, color = "black") + 
facet_grid(cols = vars(Test)) + 
scaleCOLORS + 
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank(),
	panel.spacing		=	unit(2, "lines")) + 
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) + 
scale_y_log10(
	name		=	"Mean Frame Time (ms, 15 runs)",
	breaks		=	sort(round(1000/c(1:5*200, 300, 1:5*30, 1:5*3, 1), 2)),
	limits		=	c(1, 1000),
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 15 runs)",
		trans	=	~.^-1 * 1000,
		breaks		=	sort(c(1:5*200, 300, 1:5*30, 1:5*3, 1)),
		)
) + 
scale_x_log10(
	name			=	"Pixel Area",
	breaks			=	PIXELS[, "16:9"],
	labels			=	labelBreakF(TYPE),
	expand			=	c(0.02, 0)
)

customSave("3DMark Time Spy - ms by Pixels")
