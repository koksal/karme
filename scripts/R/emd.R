options(warn = 1)

library(EMD)

args = commandArgs(trailingOnly = TRUE)
inValueFile = args[[1]]
inTimeFile  = args[[2]]
imfFile     = args[[3]]
residueFile = args[[4]]

xs = sapply(readLines(inValueFile), function (l) as.double(l))
ts = sapply(readLines(inTimeFile),  function (l) as.double(l))

result = emd(xs, boundary="wave")

write.table(
            result$imf,
            file = imfFile,
            sep = ",",
            row.names = FALSE,
            col.names = FALSE
            )

write.table(
            result$residue,
            file = residueFile,
            sep = ",",
            row.names = FALSE,
            col.names = FALSE
            )
