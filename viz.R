# Load needed packages
library("ggplot2")
library("gridExtra")


# importing the data
senic <- read.table(file.choose()
)
 
# Give colnames (makes work easier)

colnames(senic) <- c("ID","l_stay","age","i_risk","rcr","rcxr",
"n_beds", "msa", "region", "adc", "n_nurses","afs"
)

# solves a and b
quant <- function(X){
	q1 <- quantile(X, probs = c(0.25))
	q3 <- quantile(X, probs= c(0.75))
	# convert the q1 and q3 to vectors
	#q1 <- as.vector(q1)
	#q3 <- as.vector(q3)
	# create the bounds
	u_l <- q3+1.5*(q3-q1)
	l_l <- q1-1.5*(q3-q1)
	# Extract indices and return as vector
	 i_low <- which(X > u_l)
	i_high <- which(X < l_l)
	outliers <- c(i_low,i_high)
outliers
}


ir_outliers <- senic[quant(senic$i_risk), ]

ls_outliers <- senic[quant(senic$l_stay), ]

age_outliers <- senic[quant(senic$age), ]

rcr_outliers <- senic[quant(senic$rcr), ]
rcxr_outliers <- senic[quant(senic$rcxr), ]
n_beds_outliers <- senic[quant(senic$n_beds), ]
msa_outliers <- senic[quant(senic$msa), ]
adc_outliers <- senic[quant(senic$adc), ]
n_nurses_outliers <- senic[quant(senic$n_nurses), ]
afs_outliers <- senic[quant(senic$afs), ]

#Plots for all variables

ir_density <- ggplot() +
	geom_density(data = senic, aes(x = i_risk))+
	geom_point(data = ir_outliers, aes(x = ir_outliers$i_risk, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
ir_density


ls_density <- ggplot() +
  geom_density(data = senic, aes(x = l_stay))+
  geom_point(data = ls_outliers, aes(x = ls_outliers$l_stay, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
ls_density

age_density <- ggplot() +
  geom_density(data = senic, aes(x = age))+
  geom_point(data = age_outliers, aes(x = age_outliers$age, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
age_density

rcr_density <- ggplot() +
  geom_density(data = senic, aes(x = rcr))+
  geom_point(data = rcr_outliers, aes(x = rcr_outliers$rcr, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
rcr_density

rcxr_density <- ggplot() +
  geom_density(data = senic, aes(x = rcxr))+
  geom_point(data = rcxr_outliers, aes(x = rcxr_outliers$rcxr, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
rcxr_density

n_beds_density <- ggplot() +
  geom_density(data = senic, aes(x = n_beds))+
  geom_point(data = n_beds_outliers, aes(x = n_beds_outliers$n_beds, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
n_beds_density

msa_density <- ggplot() +
  geom_density(data = senic, aes(x = msa))+
  geom_point(data = ir_outliers, aes(x = msa_outliers$msa, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
msa_density

adc_density <- ggplot() +
  geom_density(data = senic, aes(x = adc))+
  geom_point(data = adc_outliers, aes(x = adc_outliers$i_risk, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
adc_density

n_nurses_density <- ggplot() +
  geom_density(data = senic, aes(x = n_nurses))+
  geom_point(data = n_nurses_outliers, aes(x = n_nurses_outliers$n_nurses, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
n_nurses_density

#check afc_density
afc_density <- ggplot() +
  geom_density(data = senic, aes(x = afc))+
  geom_point(data = afc_outliers, aes(x = afc_outliers$afc, y = 0), shape=23, fill="red", color="darkblue", size=3.5)
afc_density

# dependence 
# Good idea here to make bed factors

ir_dep_nurse <- ggplot(senic, aes(x = senic$n_nurse , y=senic$i_risk,
	color =senic$n_beds)
	)+ 
	geom_point()

ir_dep_nurse

#Produce graphs of the same kind as in step 3 but for all other quantitative variables
#in the data (aes_string() can be useful here). Put these graphs into one
#(hint: arrangeGrob() in gridExtra package can be used) and make some analysis.

grid.arrange(ir_density, ls_density, age_density, rcr_density, rcxr_density, n_beds_density, adc_density, n_nurses_density, ncol=4)
#check msa_density


#plotly
ggplotly(ir_density = ggplot2::last_plot(), width = NULL, height = NULL,
         tooltip = "all", dynamicTicks = FALSE, layerData = 1,
         originalData = TRUE, source = "A")








