#install.packages(c("sf", "osrm", "readxl", "lpSolve"))
library(sf); library(osrm); library(readxl); library(lpSolve)

setwd("C:/Duomenys/")
df_list <- setNames(lapply(1:2, function(i) read_excel("Adresai.xlsx", sheet = i)),
                    c("df_resp", "df_kl"))
df_resp <- data.frame(df_list$df_resp)
df_kl <- data.frame(df_list$df_kl)

apj_list <- list()

for (j in 1:nrow(df_kl)) {
  route_list <- lapply(1:nrow(df_resp), function(i) {
    osrmRoute(
      src = c(df_resp$Long[i], df_resp$Lat[i]),
      dst = c(df_kl$Long[j], df_kl$Lat[j]),
      overview = FALSE
    )
  })
  
  route_rb <- do.call(rbind, route_list)
  apj <- cbind(df_resp, route_rb)
  
  colnames(apj) <- c("Adresas", "Lat", "Long",
                     paste0("Laikas_V", j),
                     paste0("Atstumas_V", j))
  
  apj_list[[j]] <- apj
}

apj_final <- Reduce(function(x, y) cbind(x, y[ , 4:5]), apj_list)
apj_final

#DELETEME:
#write.table(apj_final, file = "apj_final.txt", row.names = FALSE, sep = "\t", quote = FALSE)
apj_final_READ <- read.table("apj_final.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
head(apj_final_READ)

ats_mat <- apj_final[ , seq(5, ncol(apj_final), by = 2)]
C <- as.vector(as.matrix(ats_mat))

n1 <- length(apj_final$Adresas)
n2 <- length(df_kl$Kodas)

A_list <- lapply(1:n2, function(i) {
  m <- matrix(0, nrow = n2, ncol = n1)
  m[i, ] <- 1
  return(m)
})
A_1 <- do.call(cbind, A_list)
t(A_1)
A_2 <- diag(n1)
A_n <- rbind(A_1, do.call(cbind, replicate(n2, A_2, simplify = FALSE)))

k <- 11
A <- rbind(A_n, rep(1, times = n1*n2)) 
B <- c(rep(20, times = max(n2 - 1, 0)), k, rep(1, times = n1), n1)

dir <- c(rep("<=", times = n2), rep("=", times = n1+1))

opt <- lp(direction = "min",
          objective.in = C,
          const.mat = A,
          const.dir = dir,
          const.rhs = B,
          all.bin = T)

mat <- matrix(opt$solution, ncol = n1, byrow = TRUE)
rowSums(mat)

#############

ats_mat2 <- apj_final[ , seq(4, ncol(apj_final), by = 2)]
C2 <- as.vector(as.matrix(ats_mat2))

opt2 <- lp(direction = "min",
          objective.in = C2,
          const.mat = A,
          const.dir = dir,
          const.rhs = B,
          all.bin = T)
mat2 <- matrix(opt2$solution, ncol = n1, byrow = TRUE)
rowSums(mat2)

#############

assigned_matrix <- t(mat)
assigned_matrix2 <- t(mat2)

interviewer_labels <- paste0("V", seq_len(ncol(assigned_matrix)))

colnames(assigned_matrix) <- paste0("Ats_", interviewer_labels)
colnames(assigned_matrix2) <- paste0("Laik_", interviewer_labels)

apj_final_labeled <- cbind(apj_final_READ, assigned_matrix, assigned_matrix2)
apj_final_labeled$Klausejas_Ats <- apply(apj_final_labeled[, grep("^Ats_V", names(apj_final_labeled))], 1, function(row) {
  paste0("V", which(row == 1)[1])
})

apj_final_labeled$Klausejas_Laik <- apply(apj_final_labeled[, grep("^Laik_V", names(apj_final_labeled))], 1, function(row) {
  paste0("V", which(row == 1)[1])
})
apj_final_labeled 

#############

par(mgp = c(2, 0.7, 0), mar = c(4, 6, 3, 2) + 0.1)

image(t(mat),
      col = hcl.colors(20, "Blues"),
      axes = FALSE,
      xlab = "Adresai",
      ylab = "")

label_positions <- seq(0, 1, length.out = n2)
labels <- paste("Klausėjas V_", 1:n2, sep = "")

mtext(text = labels,
      side = 2,
      line = 0.5,
      at = label_positions,
      las = 1,
      cex = 0.8)
abline(h = label_positions, col = "white", lwd = 0.5)

#############

par(mgp = c(2, 0.7, 0), mar = c(4, 6, 3, 2) + 0.1)

image(t(mat2),
      col = hcl.colors(20, "Blues"),
      axes = FALSE,
      xlab = "Adresai",
      ylab = "")

label_positions <- seq(0, 1, length.out = n2)
labels <- paste("Klausėjas V_", 1:n2, sep = "")

mtext(text = labels,
      side = 2,
      line = 0.5,
      at = label_positions,
      las = 1,
      cex = 0.8)
abline(h = label_positions, col = "white", lwd = 0.5)

#############

#install.packages("leaflet")
library(leaflet)
library(dplyr)

#Pradinis pasiskirstymas
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(c(apj_final_labeled$Long, df_kl$Long)), lat = mean(c(apj_final_labeled$Lat, df_kl$Lat)), zoom = 11.5)

map <- map %>%
  addCircleMarkers(lng = apj_final_labeled$Long, lat = apj_final_labeled$Lat,
                   popup = apj_final_labeled$Adresas,
                   color="black",  fillColor="red", radius = 8, fillOpacity = 0.8)

map <- map %>%
  addCircleMarkers(lng = df_kl$Long, lat = df_kl$Lat,
                   popup = df_kl$Kodas,
                   color = "black",  fillColor="darkblue", radius = 8, fillOpacity = 0.8)

map

#############

interviewer_colors <- c("red", "blue", "darkgreen", "purple", "yellow", "darkorange", "white")
names(interviewer_colors) <- paste0("V", 1:n2)

create_map <- function(data, prefix) {
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = mean(data$Long), lat = mean(data$Lat), zoom = 11.5)
  
  for (i in 1:n2) {
    col_name <- paste0(prefix, "V", i)
    assigned_rows <- data[data[[col_name]] == 1, ]
    
    map <- map %>%
      addCircleMarkers(data = assigned_rows,
                       lng = ~Long, lat = ~Lat,
                       popup = ~Adresas,
                       color = "black",
                       fillColor = interviewer_colors[[paste0("V", i)]],
                       label = paste0("V", i),
                       group = paste0("V", i),
                       radius = 8, fillOpacity = 0.8)
    
  }
  
  map %>%
    addLayersControl(overlayGroups = paste0("V", 1:n2),
                     options = layersControlOptions(collapsed = FALSE))
}

map_distance <- create_map(apj_final_labeled, prefix = "Ats_")
map_time     <- create_map(apj_final_labeled, prefix = "Laik_")

map_distance
map_time




