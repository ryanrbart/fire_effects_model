
# Some code for patching bad runs

# Read in allsim file
var_names <- c("leafc")
path <- RHESSYS_ALLSIM_DIR_2.1_P300_STAND1

x <- read_tsv(file.path(path, var_names), skip = 2, col_names = FALSE,
              col_types = cols(X1 = col_skip()))

# Identify rows with na
d <- sapply(x, function(x) sum(is.na(x)))
d[d>0]
na_rows <- names(d[d>0])
y <- x[names(d[d>0])]
View(y)


happy <-  read.csv(RHESSYS_PAR_SOBOL_2.1_P300, header = TRUE) 
View(happy)
happy2 <- bind_cols(run = sapply(seq_len(length(happy[,1])),
                                 function (x) paste("X",as.character(x+1),sep="")),
                    happy)




happy3 <- rbind(
happy2[happy2$run == na_rows[1],],
happy2[happy2$run == na_rows[2],],
happy2[happy2$run == na_rows[3],],
happy2[happy2$run == na_rows[4],],
happy2[happy2$run == na_rows[5],],
happy2[happy2$run == na_rows[6],],
happy2[happy2$run == na_rows[7],],
happy2[happy2$run == na_rows[8],]
)


View(happy3)
# Happy 3 may be used for patching




