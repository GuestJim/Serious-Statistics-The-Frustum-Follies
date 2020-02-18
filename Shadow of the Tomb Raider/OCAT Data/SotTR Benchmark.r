library(readr)
library(ggplot2)
library(tidyr)

gameGAQF	=	"SSFF"

RATIO	=	c(
"16:9",
"4:3",
"5:4",
"16:10",
"21:9",
"9:16"
)
TYPE	=	c(
"1280x720",
"1280x1024",
"1600x900",
"1680x1050",
"1920x1080"
)
SCALES	=	2:10/10

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"
gWIDTH		=	21
gHEIGH		=	9

ratioCOLOR	=	c("16:9"	=	"#ff0000",
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

ratioAREAS				=	(16/9)/c(16/9, 4/3, 5/4, 16/10, 21/9, 9/16)
names(ratioAREAS)		=	RATIO
ratioAREAS["21:9"]		=	ratioAREAS["21:9"]^-1
ratioAREAS				=	as.data.frame(ratioAREAS)
ratioAREAS$Ratio		=	RATIO
colnames(ratioAREAS)	=	c("Area", "Ratio")
#	because it is along a different axis the 9:16 area must be inverted

areaCOLOR	=	c(
	"1"					=	"#ff0000",
	"1.11111111111111"	=	"#0000ff",
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

#	update these list if you are using different ratios and resolutions

if (interactive())	{
	setwd("E:/Users/Jim/My Documents/OCC/@Reviews/Serious Statistics FF/Shadow of the Tomb Raider/OCAT Data")
}	else	{
	pdf(NULL)
}

resultsFull	=	read_csv("SotTR Benchmark.csv")

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
	"x1024"	=	x1024,
	"x900"	=	x900,
	"x1050"	=	x1050,
	"x1080"	=	x1080
)

PIXELS	=	rbind(
	DIMS$x720[, "Width"]	*	DIMS$x720[, "Height"],
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

StErr		=	function(DATA, r = 2)	round(sd(DATA) / sqrt(length(DATA)), r)
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
		Ratio	=	results$Ratio,
		Type	=	results$Type,
		Scale	=	results$Scale,
		API		=	results$API,
		Area	=	results$Area,
		Pixels	=	results$Pixels
		)
DATAS	=	list(
		CPU_Game	=	results$CPU_Game,
		CPU_Render	=	results$CPU_Render,
		GPU			=	results$GPU,
		FPS			=	results$FPS
		)

dataMEAN	=	aggregate(DATAS, GROUPS, roundMEAN)
dataERROR	=	aggregate(DATAS, GROUPS, StErr)

dataGEO		=	aggregate(DATAS, list(Ratio = results$Ratio), meanGEO)
#	may want to use a different approach, because of how weighted the results may be to one or the other area

# dataMEAN	=	dataMEAN[rev(order(dataMEAN$Scale)), ]

dataALL		=	merge(dataMEAN, dataERROR, by = c("Ratio", "Type", "Scale", "Area", "Pixels", "API"), suffixes = c("_Mean", "_Error"), sort = FALSE)

dataALL$Ratio	=	factor(dataALL$Ratio,	levels = RATIO,	ordered = TRUE)
dataALL$Type	=	factor(dataALL$Type,	levels = TYPE,	ordered = TRUE)
dataALL$API		=	factor(dataALL$API,						ordered = TRUE)

longMEAN	=	pivot_longer(dataMEAN,
				cols			=	7:10,
				names_to		=	"Measurement",
				names_ptypes	=	list(Measurement = factor(ordered = TRUE)),
				values_to		=	"Mean"
)

longERROR	=	pivot_longer(dataERROR,
				cols			=	7:10,
				names_to		=	"Measurement",
				names_ptypes	=	list(Measurement = factor(ordered = TRUE)),
				values_to		=	"Error"
)


longALL	=	merge(longMEAN, longERROR, by = c("Ratio", "Type", "Scale", "Area", "Pixels", "API", "Measurement"))


levels(longALL$Measurement)	=	rem_(longALL$Measurement)
longALL						=	longALL[order(longALL$Measurement), ]

longALL$Ratio	=	factor(longALL$Ratio,	levels = RATIO,	ordered = TRUE)
longALL$Type	=	factor(longALL$Type,	levels = TYPE,	ordered = TRUE)
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

htmlALL	=	dataALL
colnames(htmlALL)	=	rem_(colnames(htmlALL))
writeOCC(htmlALL,	"dataALL",	"SSFF - SotTR")
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


#	FPS by Ratio and Scale
ggplot(data = dataALL, aes(x = Scale, y = FPS_Mean, group = Ratio, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - FPS (FPS)\nGreater is better") + labs(caption = "RTX 2060") +
geom_col(position = DODGE) +
geom_errorbar(aes(ymin = FPS_Mean - FPS_Error,	ymax = FPS_Mean + FPS_Error),
	position	=	DODGE,
	width		=	0.05,
	color		=	"magenta") +
facet_grid(cols = vars(Type), rows = vars(API), switch = "y") +
scaleCOLORS +
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank()) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Rate (FPS, 3 runs)",
	breaks		=	0:5*30,
	limits		=	c(0, 125),
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Time (ms, 3 runs)",
		labels	=	FPS2ms
	)
) +
scale_x_continuous(
	name			=	"Resolution Scale",
	breaks			=	SCALES,
	labels			=	labelBreakF,
	expand			=	c(0.02, 0),
	trans			=	"reverse"
)

customSave("SotTR - FPS by Scale", width = 16)


#	FPS by Ratio and Resolution
ggplot(data = dataALL, aes(x = Type, y = FPS_Mean, group = Ratio, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - FPS (FPS)\nGreater is better") + labs(caption = "RTX 2060") +
geom_col(position = DODGE) +
geom_errorbar(aes(ymin = FPS_Mean - FPS_Error,	ymax = FPS_Mean + FPS_Error),
	position	=	DODGE,
	width		=	0.05,
	color		=	"magenta") +
facet_grid(cols = vars(Scale), rows = vars(API), switch = "y") +
scaleCOLORS +
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank(),
	panel.spacing		=	unit(1.25, "lines"),
	axis.text.x			=	element_text(angle = -45, hjust = 0)
) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Rate (FPS, 3 runs)",
	breaks		=	0:5*30,
	limits		=	c(0, 125),
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Time (ms, 3 runs)",
		labels	=	FPS2ms
	)
) +
scale_x_discrete(
	name			=	"Resolution",
	expand			=	c(0.02, 0)
)

customSave("SotTR - FPS by Res", width = 16)


#	CPU_Render by Ratio and Scale
ggplot(data = dataALL, aes(x = Scale, y = CPU_Render_Mean, group = Ratio, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - CPU Render (FPS)\nGreater is better") + labs(caption = "RTX 2060") +
geom_col(position = DODGE) +
geom_errorbar(aes(ymin = CPU_Render_Mean - CPU_Render_Error,	ymax = CPU_Render_Mean + CPU_Render_Error),
	position	=	DODGE,
	width		=	0.05,
	color		=	"magenta") +
facet_grid(cols = vars(Type), rows = vars(API), switch = "y") +
scaleCOLORS +
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank()) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean CPU Render Rate (FPS, 3 runs)",
	breaks		=	0:6*30,
	limits		=	c(0, 180),
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean CPU Render Time (ms, 3 runs)",
		labels	=	FPS2ms
	)
) +
scale_x_continuous(
	name			=	"Resolution Scale",
	breaks			=	SCALES,
	labels			=	labelBreakF,
	expand			=	c(0.02, 0),
	trans			=	"reverse"
)

customSave("SotTR - CPU_Render by Scale", width = 16)


#	CPU_Render by Ratio and Resolution
ggplot(data = dataALL, aes(x = Type, y = CPU_Render_Mean, group = Ratio, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - CPU Render (FPS)\nGreater is better") + labs(caption = "RTX 2060") +
geom_col(position = DODGE) +
geom_errorbar(aes(ymin = CPU_Render_Mean - CPU_Render_Error,	ymax = CPU_Render_Mean + FPS_Error),
	position	=	DODGE,
	width		=	0.05,
	color		=	"magenta") +
facet_grid(cols = vars(Scale), rows = vars(API), switch = "y") +
scaleCOLORS +
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank(),
	panel.spacing		=	unit(1.25, "lines"),
	axis.text.x			=	element_text(angle = -45, hjust = 0)
) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean CPU Render Rate (FPS, 3 runs)",
	breaks		=	0:6*30,
	limits		=	c(0, 180),
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean CPU Render Time (ms, 3 runs)",
		labels	=	FPS2ms
	)
) +
scale_x_discrete(
	name			=	"Resolution",
	expand			=	c(0.02, 0)
)

customSave("SotTR - CPU_Render by Res", width = 16)


#	All faceted by Resolution
ggplot(data = longALL, aes(x = Scale, y = Mean, group = Ratio, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - FPS (FPS)\nGreater is better") + labs(caption = "RTX 2060") +
geom_col(position = DODGE) +
geom_errorbar(aes(ymin = Mean - Error,	ymax = Mean + Error),
	position	=	DODGE,
	width		=	0.05,
	color		=	"magenta") +
facet_grid(cols = vars(Type), rows = vars(API, Measurement), switch = "y") +
scaleCOLORS +
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank(),
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Rate (FPS, 3 runs)",
	breaks		=	0:6*30,
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Time (ms, 3 runs)",
		labels	=	FPS2ms
	)
) +
scale_x_continuous(
	name			=	"Resolution Scale",
	breaks			=	SCALES,
	labels			=	labelBreakF,
	expand			=	c(0.02, 0),
	trans			=	"reverse"
)

customSave("SotTR - All by Scale", width = 16, height = 16)


#	All faceted by Scale
ggplot(data = longALL, aes(x = Type, y = Mean, group = Ratio, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - FPS (FPS)\nGreater is better") + labs(caption = "RTX 2060") +
geom_col(position = DODGE) +
geom_errorbar(aes(ymin = Mean - Error,	ymax = Mean + Error),
	position	=	DODGE,
	width		=	0.05,
	color		=	"magenta") +
facet_grid(cols = vars(Scale), rows = vars(API, Measurement), switch = "y") +
scaleCOLORS +
theme(
	legend.position		=	"bottom",
	panel.grid.major.x	=	element_blank(),
	panel.spacing		=	unit(1.25, "lines"),
	axis.text.x			=	element_text(angle = -45, hjust = 0)) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Rate (FPS, 3 runs)",
	breaks		=	0:6*30,
	expand		=	c(0.02, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Time (ms, 3 runs)",
		labels	=	FPS2ms
	)
) +
scale_x_discrete(
	name			=	"Resolution",
	expand			=	c(0.02, 0)
)

customSave("SotTR - All by Res", width = 16, height = 16)


#	Frame time
ggplot(data = dataALL, aes(x = Pixels*Scale, y = 1000/FPS_Mean, group = Type, color = Type, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - FPS (ms)\nLower is better") + labs(caption = "RTX 2060") +
geom_line() +
geom_point(shape = 21) +
facet_grid(rows = vars(API), switch = "y", scales = "free_y") +
scale_fill_manual(	values = ratioCOLOR,	name = "Aspect Ratio") +
scale_color_manual(	values = typeCOLOR,		name = "Resolution") +
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank()) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time (ms, 3 runs)",
	breaks		=	1000/(6:12*10),
	labels		=	round2,
	expand		=	c(0.02, 1),
	# trans		=	"log10",
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 3 runs)",
		labels		=	ms2FPS
		)
) +
scale_x_log10(
	name			=	"Pixels (with Resolution Scale)",
	breaks			=	1920*1080*SCALES,
	labels			=	paste0(c("", "\n"), "1920x1080 at ", SCALES),
	expand			=	c(0.02, 0),
	sec.axis		=	dup_axis(
		breaks		=	1280*720*SCALES,
		labels		=	paste0("1280x720 at ", SCALES, c("", "\n"))
	)
)

customSave("SotTR - FPS Time by Pixels")


#	CPU Render time
ggplot(data = dataALL, aes(x = Pixels*Scale, y = 1000/CPU_Render_Mean, group = Type, color = Type, fill = Ratio)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - CPU Render (ms)\nLower is better") + labs(caption = "RTX 2060") +
geom_line() +
geom_point(shape = 21) +
facet_grid(rows = vars(API), switch = "y", scales = "free_y") +
scale_fill_manual(	values = ratioCOLOR,	name = "Aspect Ratio") +
scale_color_manual(	values = typeCOLOR,		name = "Resolution") +
theme(
	legend.position		=	"bottom",
	# panel.grid.minor.x	=	element_blank()
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time (ms, 3 runs)",
	breaks		=	1000/(5:17*10),
	labels		=	round2,
	expand		=	c(0.02, 1),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 3 runs)",
		labels		=	ms2FPS
		)
) +
scale_x_log10(
	name			=	"Pixels (with Resolution Scale)",
	breaks			=	1920*1080*SCALES,
	labels			=	paste0(c("", "\n"), "1920x1080 at ", SCALES),
	expand			=	c(0.02, 0),
	sec.axis		=	dup_axis(
		breaks		=	1280*720*SCALES,
		labels		=	paste0("1280x720 at ", SCALES, c("", "\n"))
	)
)

customSave("SotTR - CPU_Render Time by Pixels")


#	Frame time by Area, Resolution groups
ggplot(data = dataALL, aes(x = Pixels*Scale, y = (1000/FPS_Mean)/(Pixels*Scale), group = Type, color = Ratio, fill = Type)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - FPS (ms per Pixel)\nLower is better") + labs(caption = "RTX 2060") +
geom_line() +
geom_point(shape = 21, color = "black") +
facet_grid(rows = vars(API), switch = "y") +
scale_color_manual(	values = ratioCOLOR,	name = "Aspect Ratio") +
scale_fill_manual(	values = typeCOLOR,		name = "Resolution") +
theme(
	legend.position		=	"bottom",
	# panel.grid.major.x	=	element_blank()
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time per Pixel per Frame (ms / Pixel / Frame, 3 runs)",
	breaks		=	0:8*1e-5,
	expand		=	c(0.02, 0),
	trans		=	"log10",
	sec.axis	=	dup_axis(
		labels	=	sci2norm
		)
) +
scale_x_log10(
	name			=	"Pixels (with Resolution Scale)",
	breaks			=	1920*1080*SCALES,
	labels			=	paste0("1920x1080 at ", SCALES, c("", "\n")),
	expand			=	c(0.02, 0.05),
	sec.axis		=	dup_axis(
		breaks		=	1280*720*SCALES,
		labels		=	paste0("1280x720 at ", SCALES, c("", "\n")),
	)
)


customSave("SotTR - FPS Time per Pixel by Pixels")


#	CPU Render time by Area, Ratio groups
ggplot(data = dataALL, aes(x = Pixels*Scale, y = (1000/CPU_Render_Mean)/(Pixels*Scale), group = Ratio, color = Ratio, fill = Type)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - CPU Render (ms per Pixel)\nLower is better") + labs(caption = "RTX 2060") +
geom_line() +
geom_point(shape = 21, color = "black") +
facet_grid(rows = vars(API), switch = "y") +
scale_color_manual(	values = ratioCOLOR,	name = "Aspect Ratio") +
scale_fill_manual(	values = typeCOLOR,		name = "Resolution") +
theme(
	legend.position		=	"bottom",
	# panel.grid.major.x	=	element_blank()
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean CPU Render Time per Pixel per Frame (ms, 3 runs)",
	breaks		=	0:8*(1e-5),
	expand		=	c(0.02, 0),
	trans		=	"log10",
	sec.axis	=	dup_axis(
		labels	=	sci2norm)
) +
scale_x_log10(
	name			=	"Pixels (with Resolution Scale)",
	breaks			=	1920*1080*SCALES,
	labels			=	paste0(c("", "\n"), "1920x1080 at ", SCALES),
	expand			=	c(0.02, 0.05),
	sec.axis		=	dup_axis(
		name		=	NULL,
		breaks		=	1280*720*SCALES,
		labels		=	paste0("1280x720 at ", SCALES, c("", "\n"))
	)
)

customSave("SotTR - CPU_Render Time per Pixel by Pixels")


#	FPS time by Ratio
ggplot(data = dataALL, aes(x = Ratio, y = (1000/FPS_Mean), fill = Type)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - FPS (ms)\nLower is better") + labs(caption = "RTX 2060") +
geom_point(shape = 21, color = "black") +
facet_grid(rows = vars(API), cols = vars(Scale), switch = "y", scales = "free_y") +
scale_fill_manual(	values = typeCOLOR,		name = "Resolution") +
scale_color_manual(	values = typeCOLOR,		name = "Resolution") +
theme(
	legend.position		=	"bottom",
	# panel.grid.major.x	=	element_blank()
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time (ms, 3 runs)",
	breaks		=	1000/(0:6*30),
	labels		=	round(1000/(0:6*30), 2),
	expand		=	c(0.02, 2),
	# limits		=	c(1000/180, 1000/60),
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate (FPS, 3 runs)",
		labels	=	0:6*30
	)
) + 
scale_x_discrete(
	name		=	"Aspect Ratio"
)

customSave("SotTR - FPS Time by Ratio")


#	CPU_Render time by Ratio
ggplot(data = dataALL, aes(x = Ratio, y = (1000/CPU_Render_Mean), fill = Type)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - CPU Render (ms)\nLower is better") + labs(caption = "RTX 2060") +
geom_point(shape = 21, color = "black") +
facet_grid(rows = vars(API), cols = vars(Scale), switch = "y", scales = "free_y") +
scale_fill_manual(	values = typeCOLOR,		name = "Resolution") +
scale_color_manual(	values = typeCOLOR,		name = "Resolution") +
theme(
	legend.position		=	"bottom",
	# panel.grid.major.x	=	element_blank()
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean CPU Render Time (ms, 3 runs)",
	breaks		=	1000/(0:6*30),
	labels		=	round(1000/(0:6*30), 2),
	expand		=	c(0.02, 2),
	# limits		=	c(1000/180, 1000/60),
	sec.axis	=	dup_axis(
		name	=	"Mean CPU Render Rate (FPS, 3 runs)",
		labels	=	0:6*30
	)
) + 
scale_x_discrete(
	name		=	"Aspect Ratio"
)

customSave("SotTR - CPU_Render Time by Ratio")


#	try a graph with x = Area and y = (1000/Mean)/Pixels
ggplot(data = dataALL, aes(x = Area, y = (1000/FPS_Mean)/(Pixels*Scale), fill = Type)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - FPS (ms)\nLower is better") + labs(caption = "RTX 2060") +
geom_hline(aes(yintercept = (1000/FPS_Mean)/(Pixels*Scale), color = Type)) + 
geom_point(shape = 21, color = "black") +
facet_grid(rows = vars(API), cols = vars(Scale), switch = "y", scales = "free_y") +
scale_fill_manual(	values = typeCOLOR,		name = "Resolution") +
scale_color_manual(	values = typeCOLOR,		name = "Resolution") +
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank()
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean Frame Time per Pixel per Frame (ms / Pixel / Frame, 3 runs)",
	breaks		=	c(5e-6, 1:7*1e-5),
	limits		=	c(5e-6, NA),
	trans		=	"log10",
	sec.axis	=	dup_axis(
		name	=	"Mean Frame Rate per Pixel per Frame (ms / Pixel / Frame, 3 runs)",
		labels	=	sci2norm
	)
) + 
scale_x_continuous(
	name	=	"Area Relative to 16:9",
	breaks	=	unique(longALL$Area),
	labels	=	unique(longALL$Ratio),
	expand	=	c(0.02, 0.025),
	# trans	=	"log10"
)

customSave("SotTR - FPS Time per Pixel by Area")


ggplot(data = dataALL, aes(x = Area, y = (1000/CPU_Render_Mean)/(Pixels*Scale), fill = Type)) +
ggtitle("Serious Statistics: The Frustum Follies", subtitle = "Shadow of the Tomb Raider - CPU Render (ms)\nLower is better") + labs(caption = "RTX 2060") +
geom_hline(aes(yintercept = (1000/CPU_Render_Mean)/(Pixels*Scale), color = Type)) + 
geom_point(shape = 21, color = "black") +
facet_grid(rows = vars(API), cols = vars(Scale), switch = "y", scales = "free_y") +
scale_fill_manual(	values = typeCOLOR,		name = "Resolution") +
scale_color_manual(	values = typeCOLOR,		name = "Resolution") +
theme(
	legend.position		=	"bottom",
	panel.grid.minor.x	=	element_blank()
	) +
guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
scale_y_continuous(
	name		=	"Mean CPU Render Time per Pixel per Frame (ms / Pixel / Frame, 3 runs)",
	breaks		=	c(5e-6, 1:7*1e-5),
	limits		=	c(3e-6, NA),
	expand		=	c(0.03, 0),
	sec.axis	=	dup_axis(
		name	=	"Mean CPU Render Rate per Pixel per Frame (ms / Pixel / Frame, 3 runs)",
		labels	=	sci2norm
	)
) + 
scale_x_continuous(
	name	=	"Area Relative to 16:9",
	breaks	=	unique(longALL$Area),
	labels	=	unique(longALL$Ratio),
	expand	=	c(0.02, 0.025),
	# trans	=	"log10"
)

customSave("SotTR - CPU_Render Time per Pixel by Area")
