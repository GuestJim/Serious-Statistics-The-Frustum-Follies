library(readr)
library(ggplot2)
library(tidyr)

gameGAQF	=	"SSFF"

RATIO	=	c(
"9:16",
"5:4",
"4:3",
"16:10",
"16:9",
"21:9"
)
TYPE	=	c(
"1280x720",
"1280x960",
"1280x1024",
"1600x900",
"1680x1050",
"1920x1080"
)

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"
gWIDTH		=	21
gHEIGH		=	9

ratioCOLOR	=	c(
	"16:9"	=	"#ff0000",
	"4:3"	=	"#0000ff",
	"5:4"	=	"#00ffff",
	"16:10"	=	"#ff7f7f",
	"21:9"	=	"#00ff00",
	"9:16"	=	"#ffff00"
	)
	

typeCOLOR	=	c(
	"1280x720"	=	"#e76bf3",
	"1280x960"	=	"#a3a500",
	"1280x1024"	=	"#00b0f6",
	"1600x900"	=	"#00bf7d",
	"1680x1050"	=	"#619cff",
	"1920x1080"	=	"#f8766d"
)
#	update these list if you are using different ratios and resolutions

RATIO	=	c(
"9:16",
"5:4",
"4:3",
"16:10",
"16:9",
"21:9"
)


ratioAREAS	=	c(9/16, 5/4, 4/3, 16/10, 16/9, 21/9)/(16/9)
names(ratioAREAS)	=	RATIO
ratioAREAS["9:16"]	=	ratioAREAS["9:16"]^-1
#	because it is along a different axis the 9:16 area must be inverted
ratioAREAS				=	as.data.frame(ratioAREAS)
ratioAREAS$Ratio		=	RATIO
colnames(ratioAREAS)	=	c("Area", "Ratio")

areaCOLOR	=	c(
	"1"			=	"#ff0000",
	"0.75"		=	"#0000ff",
	"0.703125"	=	"#00ffff",
	"0.9"		=	"#ff7f7f",
	"1.3125"	=	"#00ff00",
	"3.160494"	=	"#ffff00"
	)

areaLABELS	=	c(
	"16:9"	=	1,
	"4:3"	=	0.75,
	"5:4"	=	0.703125,
	"16:10"	=	0.9,
	"21:9"	=	1.3125,
	"9:16"	=	3.160494
	)


if (interactive())	{
	setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics FF/Serious Sam/OCAT Data")
}	else	{
	pdf(NULL)
}

resultsFull	=	read_csv("Serious Sam Benchmark.csv")

results	=	resultsFull


results$Type	=	factor(results$Type,	levels = TYPE,	ordered = TRUE)
results$Run		=	factor(results$Run,						ordered = TRUE)
results$Ratio	=	factor(results$Ratio,	levels = RATIO,	ordered = TRUE)


dimNAME	=	function(DATA, ratio = RATIO)	{
	rownames(DATA)	=	ratio
	colnames(DATA)	=	c("Width", "Height")
	return(DATA)
}

x720	=	dimNAME(t(data.frame(
	c(1280,720)
)),	"16:9")

x960	=	dimNAME(t(data.frame(
	c(1280,960)
)),	"4:3")

x1024	=	dimNAME(t(data.frame(
	c(1280,1024)
)),	"5:4")

x900	=	dimNAME(t(data.frame(
	c(1600,900)
)),	"16:9")

x1050	=	dimNAME(t(data.frame(
	c(1680,1080)
)),	"16:10")

x1080	=	dimNAME(t(data.frame(
	c(1920,1080)
)),	"16:9")

DIMS	=	list(
	"x720"	=	x720,
	"x960"	=	x960,
	"x1024"	=	x1024,
	"x900"	=	x900,
	"x1050"	=	x1050,
	"x1080"	=	x1080
)

PIXELS	=	rbind(
	DIMS$x720[, "Width"]	*	DIMS$x720[, "Height"],
	DIMS$x960[, "Width"]	*	DIMS$x960[, "Height"],
	DIMS$x1024[, "Width"]	*	DIMS$x1024[, "Height"],
	DIMS$x900[, "Width"]	*	DIMS$x900[, "Height"],
	DIMS$x1050[, "Width"]	*	DIMS$x1050[, "Height"],
	DIMS$x1080[, "Width"]	*	DIMS$x1080[, "Height"]
)
rownames(PIXELS)	=	TYPE
PIXELS				=	as.data.frame(PIXELS)
colnames(PIXELS)	=	"Pixels"
PIXELS$Type			=	TYPE

results	=	merge(ratioAREAS, results, by = "Ratio")

results	=	merge(PIXELS, results, by = c("Type"), sort = FALSE)


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

StErr		=	function(DATA, r = 4)	round(sd(DATA) / sqrt(length(DATA)), r)
#	Standard Error function

customSave	=	function(type="", device=ggdevice, plot = last_plot(), width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	if	(device	==	"png"	|	device == "both")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".png"), plot = plot, device="png", width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf"	|	device == "both")	{
		ggsave(filename=paste0(gameGAQF, " - ", type, ".pdf"), plot = plot, device="pdf", width=width, height=height)
	}
}

GROUPS	=	list(
		Type	=	results$Type,
		Pixels	=	results$Pixels,
		Ratio	=	results$Ratio,
		FOV		=	results$FOV,
		Area	=	results$Area,
		API		=	results$API
		)
DATAS	=	list(
		FPS		=	results$FPS,
		ms		=	results$ms
		)

dataMEAN	=	aggregate(DATAS, GROUPS, roundMEAN)
dataERROR	=	aggregate(DATAS, GROUPS, StErr)

dataGEO		=	aggregate(DATAS, list(Ratio = results$Ratio), meanGEO)
#	may want to use a different approach, because of how weighted the results may be to one or the other area


dataALL		=	merge(dataMEAN, dataERROR, by = c("Type", "Pixels", "Ratio", "FOV", "Area", "API"), suffixes = c("_Mean", "_Error"), sort = FALSE)


dataALL$Type	=	factor(dataALL$Type,	levels = TYPE,	ordered = TRUE)
dataALL$Ratio	=	factor(dataALL$Ratio,	levels = RATIO,	ordered = TRUE)
dataALL$API		=	factor(dataALL$API,						ordered = TRUE)


longMEAN	=	pivot_longer(dataMEAN,
				cols			=	7:8,
				names_to		=	"Measurement",
				names_ptypes	=	list(Measurement = factor(ordered = TRUE)),
				values_to		=	"Mean"
)

longERROR	=	pivot_longer(dataERROR,
				cols			=	7:8,
				names_to		=	"Measurement",
				names_ptypes	=	list(Measurement = factor(ordered = TRUE)),
				values_to		=	"Error"
)


longALL	=	merge(longMEAN, longERROR, by = c("Type", "Pixels", "Ratio", "FOV", "Area", "API", "Measurement"), sort = FALSE)

longALL$Type	=	factor(longALL$Type,	levels = TYPE,	ordered = TRUE)
longALL$Ratio	=	factor(longALL$Ratio,	levels = RATIO,	ordered = TRUE)
longALL$API		=	factor(longALL$API,						ordered = TRUE)


library(tableHTML)
OCCHTML	=	function(DATA, rNAMES	=	FALSE)	{
	tableHTML(DATA, rownames = rNAMES, class="OCC") %>%
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
}

writeOCC	=	function(DATA, dataNAME, name, ...)	{
	write_tableHTML(OCCHTML(DATA, ...), file = paste0(name, " - ", dataNAME,".html"))
}


htmlALL	=	dataALL[with(dataALL, order(Type)), -grep("Error", colnames(dataALL))]
colnames(htmlALL)	=	rem_(colnames(htmlALL))
writeOCC(htmlALL,	"dataALL",	"SSFF - Serious Sam")

htmlFOV	=	dataALL[with(dataALL, order(Type)), c("Type", "Ratio", "FOV")]

htmlFOV	=	pivot_wider(htmlFOV,
		names_from		=	"Type",
		values_from		=	"FOV"
	)

writeOCC(htmlFOV,	"FOV",	"SSFF - Serious Sam")


# writeOCC(AREAS,		"Areas",	"SSFF - SotTR",	rNAMES = TRUE)

# for(place in 1:length(DIMS))	{
	# writeOCC(as.data.frame(DIMS[place]),	paste0("Dimensions - ", names(DIMS)[place]),	"SSFF",	rNAMES = TRUE)
# }

DODGE	=	position_dodge(width = 1)
#	necessary for proper alignment of error bars, and that this be applied for both the bars and the coloumns

backRECT	=	function(DATA)	{
	if (is.data.frame(DATA))	lenDATA	=	nrow(DATA)
	if (is.factor(DATA))		lenDATA	=	length(DATA)

	PATTERN	=	0:(length(TYPE) / 2 - 1) * 2 + 1.5

	xmin	=	rep(PATTERN		,	length.out = lenDATA)
	xmax	=	rep(PATTERN + 1,	length.out = lenDATA)
	ymin	=	-Inf
	ymax	=	Inf

	return(as.data.frame(cbind(xmin, xmax, ymin, ymax)))
}

backRECT.log	=	function(DATA)	{
	if (is.data.frame(DATA))	lenDATA	=	nrow(DATA)
	if (is.factor(DATA))		lenDATA	=	length(DATA)

	PATTERN	=	0:(length(TYPE) / 2 - 1) * 2 + 1.5

	xmin	=	rep(PATTERN		,	length.out = lenDATA)
	xmax	=	rep(PATTERN + 1,	length.out = lenDATA)
	ymin	=	1
	ymax	=	Inf

	return(as.data.frame(cbind(xmin, xmax, ymin, ymax)))
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
		rows	=	c(rows, grep(i, DATA[, COL][[1]]))
	}
	return(rows)
}
#	this is necessary for the some graphs as a means to isolate certain results

ggplot(data = dataALL, aes(x = Ratio, y = FPS_Mean, group = Ratio, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Serious Sam Fusion 2017 - Frame Rate (FPS)\nGreater is better") + labs(caption = "RTX 2060") +
geom_col(position = DODGE) +
# geom_errorbar(aes(ymin = FPS_Mean - FPS_Error,	ymax = FPS_Mean + FPS_Error),
	# position	=	DODGE,
	# width		=	0.333,
	# color		=	"magenta") +
#	the real standard error values are much smaller, but can only be gotten by loading the 1.92 GB of original data
facet_grid(cols = vars(Type), switch = "y") +
scale_fill_manual(	values = ratioCOLOR,	name = "Aspect Ratio") +
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank(),
	# panel.spacing		=	unit(1.25, "lines"),
	# axis.text.x			=	element_text(angle = -45, hjust = 0)
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Rate (FPS, 5 runs)",
	breaks		=	0:14*30,
	limits		=	c(0, 395),
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Time (ms, 5 runs)",
		labels	=	FPS2ms)
) +
scale_x_discrete(
	name			=	"Effective Aspect Ratio",
	labels			=	labelBreakF,
	expand			=	c(0.02, 0),
	# trans			=	"reverse"
)

customSave("Serious Sam - FPS by Ratio", width = 16)


ggplot(data = dataALL, aes(x = Area, y = ms_Mean/Pixels)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Serious Sam Fusion 2017 - ms per Pixel (ms / Pixel / Frame)\nLower is better") + labs(caption = "RTX 2060") +
geom_smooth(data = results,	aes(y = ms/Pixels,	group = Type,	color = Type), se = FALSE) + 
geom_point(		aes(fill = Ratio),	shape = 21,	color = "black") + 
# facet_grid(rows = vars(Type), switch = "y", scales = "free_y", as.table = FALSE) +
scale_fill_manual(	values = ratioCOLOR,	name = "Aspect Ratio") +
scale_color_manual(	values = typeCOLOR,		name = "Aspect Ratio") +
theme(
	legend.position		=	"bottom",
	panel.spacing		=	unit(1.5, "lines"),
	# axis.text.x			=	element_text(angle = -45, hjust = 0)
	) +
guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
scale_y_continuous(
	name		=	"Mean Frame Time per Pixel per Frame (ms / Pixel / Frame, 5 runs)",
	# breaks		=	0:4*1e-6,
	# limits		=	c(0, 3.5e-6),
	limits		=	c(NA, 3e-6),
	expand		=	c(0.1, 0),
	sec.axis	=	dup_axis(
		labels	=	sci2norm
		)
) +
scale_x_continuous(
	name	=	"Area Relative to 16:9",
	breaks		=	sort(areaLABELS),
	labels		=	labelBreakF(names(sort(areaLABELS))),
	expand	=	c(0.02, 0),
	trans	=	"log10"
)

customSave("Serious Sam - ms per Pixel by Ratio", width = 12)


ggplot(data = dataALL, aes(x = Area, y = ms_Mean/Pixels)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Serious Sam Fusion 2017 - ms per Pixel (ms / Pixel / Frame)\nLower is better") + labs(caption = "RTX 2060") +
geom_smooth(data = results,	aes(y = ms/Pixels,	group = Type,	color = Type), se = FALSE) + 
geom_point(		aes(fill = Ratio),	shape = 21,	color = "black") + 
# facet_grid(rows = vars(Type), switch = "y", scales = "free_y", as.table = FALSE) +
facet_grid(rows = vars(Type), switch = "y", scales = "free_y") +
scale_fill_manual(	values = ratioCOLOR,	name = "Aspect Ratio") +
scale_color_manual(	values = typeCOLOR,		name = "Aspect Ratio") +
theme(
	legend.position		=	"bottom",
	panel.spacing		=	unit(1.5, "lines"),
	# axis.text.x			=	element_text(angle = -45, hjust = 0)
	) +
guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time per Pixel per Frame (ms / Pixel / Frame, 5 runs)",
	# breaks		=	0:4*1e-6,
	# limits		=	c(0, 3.5e-6),
	expand		=	c(0.5, 0),
	sec.axis	=	dup_axis(
		labels	=	sci2norm
		)
) +
scale_x_continuous(
	name	=	"Area Relative to 16:9",
	breaks		=	sort(areaLABELS),
	labels		=	labelBreakF(names(sort(areaLABELS))),
	expand	=	c(0.02, 0),
	trans	=	"log10"
)

# customSave("Serious Sam - ms per Pixel by Ratio Zoom", width = 12, height = 10)
#	little point to Zooming as it will like the same as the ms by Area Zoom graph since it is all the same except for dividing by Pixels, which are constant across Type

ggplot(data = dataALL, aes(x = Pixels, y = ms_Mean/Pixels, group = Ratio, color = Ratio, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Serious Sam Fusion 2017 - ms per Pixel (ms / Pixel / Frame)\nLower is better") + labs(caption = "RTX 2060") +
# geom_smooth(se = FALSE) + 
geom_smooth(data = results, aes(y = ms/Pixels), se = FALSE) + 
geom_point(shape = 21, color = "black") + 
scale_fill_manual(	values = ratioCOLOR,	name = "Effective Aspect Ratio") +
scale_color_manual(	values = ratioCOLOR,	name = "Effective Aspect Ratio") +
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank(),
	panel.spacing		=	unit(1.5, "lines"),
	# axis.text.x			=	element_text(angle = -45, hjust = 0)
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time per Pixel per Frame (ms / Pixel / Frame, 5 runs)",
	breaks		=	1:4*1e-6,
	expand		=	c(0.04, 0),
	sec.axis	=	dup_axis(
		labels	=	sci2norm
		),
	trans		=	"log10"
) +
scale_x_continuous(
	name		=	"Pixels",
	breaks		=	PIXELS$Pixels,
	labels		=	labelBreakF(PIXELS$Type),
	trans		=	"log10"
)

customSave("Serious Sam - ms per Pixel by Pixels", width = 10)


ggplot(data = dataALL, aes(x = Pixels, y = ms_Mean, group = Ratio, color = Ratio, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Serious Sam Fusion 2017 - Frame Time (ms)\nLower is better") + labs(caption = "RTX 2060") +
# geom_smooth(data = results, aes(y = ms), alpha = 0.15) + 
geom_smooth(data = results, aes(y = ms), se = FALSE) + 
geom_point(shape = 21, color = "black") + 
scale_fill_manual(	values = ratioCOLOR,	name = "Effective Aspect Ratio") +
scale_color_manual(	values = ratioCOLOR,	name = "Effective Aspect Ratio") +
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank(),
	panel.spacing		=	unit(1.5, "lines"),
	# axis.text.x			=	element_text(angle = -45, hjust = 0)
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time (ms, 5 runs)",
	breaks		=	1000/(0:15*30),
	labels		=	round2,
	limits		=	c(NA, 1000/240),
	expand		=	c(0.04, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 5 runs)",
		labels	=	ms2FPS
		),
	# trans		=	"log10"
) +
scale_x_continuous(
	name		=	"Pixels",
	breaks		=	PIXELS$Pixels,
	labels		=	labelBreakF(PIXELS$Type),
	trans		=	"log10"
)

customSave("Serious Sam - ms by Pixels", width = 10)


ggplot(data = dataALL, aes(x = Area, y = ms_Mean, group = Type, color = Type, fill = as.factor(Area))) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Serious Sam Fusion 2017 - Frame Time (ms) by Area\nLower is better") + labs(caption = "RTX 2060") +
# geom_errorbar(aes(ymin = ms_Mean - ms_Error,	ymax = ms_Mean + ms_Error),
	# width		=	0.005,
	# color		=	"magenta") +
#	the real standard error values are much smaller, but can only be gotten by loading the 1.92 GB of original data
# geom_line(	show.legend = TRUE) +
geom_smooth(data = results,	aes(y = ms), se = FALSE) + 
geom_point(shape = 21, color = "black") + 
scale_color_manual(name	=	"Resolution",	values	=	typeCOLOR) +
scale_fill_manual(
	name	=	"Aspect Ratio",
	values	=	areaCOLOR,
	breaks	=	areaLABELS,
	labels	=	names(areaLABELS),
	) +
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank(),
	panel.spacing		=	unit(1.5, "lines"),
	) +
guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
scale_y_continuous(
	name		=	"Mean Frame Time (ms, 5 runs)",
	breaks		=	seq(2.60, 4.00, by = 0.05),
	labels		=	round2,
	expand		=	c(0.02, 0.01),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 5 runs)",
		labels	=	ms2FPS)
) +
scale_x_continuous(
	name	=	"Area Relative to 16:9",
	breaks	=	ratioAREAS$Area,
	labels	=	ratioAREAS$Ratio,
	expand	=	c(0.02, 0),
	trans	=	"log10"
)

customSave("Serious Sam - ms by Area", width = 10, height = 10)


ggplot(data = dataALL, aes(x = Area, y = ms_Mean, group = Type, color = Type, fill = as.factor(Area))) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Serious Sam Fusion 2017 - Frame Time (ms) by Area\nLower is better") + labs(caption = "RTX 2060") +
# geom_errorbar(aes(ymin = ms_Mean - ms_Error,	ymax = ms_Mean + ms_Error),
	# width		=	0.005,
	# color		=	"magenta") +
#	the real standard error values are much smaller, but can only be gotten by loading the 1.92 GB of original data
# geom_line(	show.legend = FALSE) +
geom_smooth(data = results,	aes(y = ms,	group = Type,	color = Type), se = FALSE) + 
geom_point(shape = 21, color = "black") + 
scale_color_manual(	values = typeCOLOR,		name = "Resolution") +
scale_fill_manual(
	name	=	"Aspect Ratio",
	values	=	areaCOLOR,
	breaks	=	areaLABELS,
	labels	=	names(areaLABELS),
	)+
facet_grid(rows = vars(Type), scales = "free_y", switch = "y", as.table = FALSE) + 
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank(),
	panel.spacing		=	unit(1.5, "lines"),
	# axis.text.x			=	element_text(angle = -45, hjust = 0)
	) +
guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
scale_y_continuous(
	name		=	"Mean Frame Time (ms, 5 runs)",
	# breaks		=	1000/(0:13*30),
	breaks		=	seq(2.60, 4.00, by = 0.05),
	labels		=	round2,
	expand		=	c(0.02, 0.01),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 5 runs)",
		labels	=	ms2FPS)
) +
scale_x_continuous(
	name	=	"Area Relative to 16:9",
	breaks	=	unique(dataALL$Area),
	labels	=	unique(dataALL$Ratio),
	expand	=	c(0.02, 0),
	trans	=	"log10"
)

customSave("Serious Sam - ms by Area Zoom", width = 10, height = 10)
