### The problem of assigning the optimal number of addresses to interviewers ###
### Beata Borisova ###

library(osrm)
library(readxl)
library(lpSolve)

dfs <- setNames(lapply(1:2, function(i) read_excel("Opt_data.xlsx", sheet = i)),
                    c("df_resp", "df_inter"))

df_resp <- data.frame(dfs$df_resp)
df_inter <- data.frame(dfs$df_inter)

### CREATING DF ###
df_list <- list()

for (j in 1:nrow(df_inter)) {
  route_list <- lapply(1:nrow(df_resp), function(i) {
    osrmRoute(
      src = c(df_resp$Long[i], df_resp$Lat[i]),
      dst = c(df_inter$Long[j], df_inter$Lat[j]),
      overview = FALSE
    )
  })

  route_rb <- do.call(rbind, route_list)
  df <- cbind(df_resp, route_rb)

  colnames(df) <- c("Address", "Lat", "Long",
                     paste0("Time_V", j),
                     paste0("Distance_V", j))

  df_list[[j]] <- df
}

df_final <- Reduce(function(x, y) cbind(x, y[ , 4:5]), df_list)

#DELETEME:
#write.table(df_final, file = "df_final.txt", row.names = FALSE, sep = "\t", quote = FALSE)
#df_final_READ <- read.table("df_final.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

### ASSIGNING THE OPTIMAL NUMBER OF ADDRESSES TO INTERVIEWERS BASED ON DISTANCE ###
dist_mat <- df_final[ , seq(5, ncol(df_final), by = 2)]
C_dist <- as.vector(as.matrix(dist_mat))

n_address <- length(df_final$Address)
n_code <- length(df_inter$Code)

A_list <- lapply(1:n_code, function(i) {
  m <- matrix(0, nrow = n_code, ncol = n_address)
  m[i, ] <- 1
  return(m)
})

A_1 <- do.call(cbind, A_list)
A_2 <- diag(n_address)
A_n <- rbind(A_1, do.call(cbind, replicate(n_code, A_2, simplify = FALSE)))
A <- rbind(A_n, rep(1, times = n_address*n_code)) 

k <- 11
B <- c(rep(20, times = max(n_code - 1, 0)), k, rep(1, times = n_address), n_address)

dir <- c(rep("<=", times = n_code), rep("=", times = n_address+1))

opt_dist <- lp(direction = "min",
          objective.in = C_dist,
          const.mat = A,
          const.dir = dir,
          const.rhs = B,
          all.bin = T)

mat_dist_sol <- matrix(opt_dist$solution, ncol = n_address, byrow = TRUE)
rowSums(mat_dist_sol)

### ASSIGNING THE OPTIMAL NUMBER OF ADDRESSES TO INTERVIEWERS BASED ON TIME ###
time_mat <- df_final[ , seq(4, ncol(df_final), by = 2)]
C_time <- as.vector(as.matrix(time_mat))

opt_time <- lp(direction = "min",
          objective.in = C_time,
          const.mat = A,
          const.dir = dir,
          const.rhs = B,
          all.bin = T)

mat_time_sol <- matrix(opt_time$solution, ncol = n_address, byrow = TRUE)
rowSums(mat_time_sol)

### CREATING LABELED DF ###
assigned_dist <- t(mat_dist_sol)
assigned_time <- t(mat_time_sol)

inter_labels <- paste0("V", seq_len(ncol(assigned_dist)))
colnames(assigned_dist) <- paste0("Dist_LABEL_", inter_labels)
colnames(assigned_time) <- paste0("Time_LABEL_", inter_labels)

df_final_labeled <- cbind(df_final, assigned_dist, assigned_time)
df_final_labeled$Interviewer_dist <- apply(df_final_labeled[, grep("^Dist_LABEL_V", names(df_final_labeled))], 1, function(row) {
  paste0("V", which(row == 1)[1])
})
df_final_labeled$Interviewer_time <- apply(df_final_labeled[, grep("^Time_LABEL_V", names(df_final_labeled))], 1, function(row) {
  paste0("V", which(row == 1)[1])
}) 

### VISUALIZING DATA WITH LEAFLET ###
library(leaflet)
library(dplyr)

map <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(c(df_final_labeled$Long, df_inter$Long)), lat = mean(c(df_final_labeled$Lat, df_inter$Lat)), zoom = 11.5)

map <- map %>%
  addCircleMarkers(lng = df_final_labeled$Long, lat = df_final_labeled$Lat,
                   popup = df_final_labeled$Address,
                   color="black",  fillColor="red", radius = 8, fillOpacity = 0.8)

map <- map %>%
  addCircleMarkers(lng = df_inter$Long, lat = df_inter$Lat,
                   popup = df_inter$Code,
                   color = "black",  fillColor="darkblue", radius = 8, fillOpacity = 0.8)
map

### ALLOCATION OF ADDRESSES BASED ON DISTANCE ###
par(mgp = c(2, 0.7, 0), mar = c(4, 6, 3, 2) + 0.1)

image(t(mat_dist_sol),
      col = hcl.colors(20, "Blues"),
      axes = FALSE,
      xlab = "Addresses",
      ylab = "")

label_posit<- seq(0, 1, length.out = n_code)
labels <- paste("Interviewer V_", 1:n_code, sep = "")

mtext(text = labels,
      side = 2,
      line = 0.5,
      at = label_posit,
      las = 1,
      cex = 0.8)
abline(h = label_posit, col = "white", lwd = 0.5)

### ALLOCATION OF ADDRESSES BASED ON TIME ###
image(t(mat_time_sol),
      col = hcl.colors(20, "Blues"),
      axes = FALSE,
      xlab = "Addresses",
      ylab = "")

label_positions <- seq(0, 1, length.out = n_code)
labels <- paste("Interviewer V_", 1:n_code, sep = "")

mtext(text = labels,
      side = 2,
      line = 0.5,
      at = label_positions,
      las = 1,
      cex = 0.8)
abline(h = label_positions, col = "white", lwd = 0.5)

### ALLOCATION OF ADDRESSES BASED ON DISTANCE AND TIME WITH LEAFLET ###
inter_col <- c("red", "blue", "darkgreen", "purple", "yellow", "darkorange", "white")
names(inter_col) <- paste0("V", 1:n_code)

create_map <- function(data, pref) {
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = mean(data$Long), lat = mean(data$Lat), zoom = 11.5)
  
  for (i in 1:n_code) {
    col_name <- paste0(pref, "V", i)
    assigned_rows <- data[data[[col_name]] == 1, ]
    
    map <- map %>%
      addCircleMarkers(data = assigned_rows,
                       lng = ~Long, lat = ~Lat,
                       popup = ~Address,
                       color = "black",
                       fillColor = inter_col[[paste0("V", i)]],
                       label = paste0("V", i),
                       group = paste0("V", i),
                       radius = 8, fillOpacity = 0.8)
    
  }
  
  map %>%
    addLayersControl(overlayGroups = paste0("V", 1:n_code),
                     options = layersControlOptions(collapsed = FALSE))
}

map_dist <- create_map(df_final_labeled, pref= "Dist_LABEL_")
map_time <- create_map(df_final_labeled, pref = "Time_LABEL_")
map_dist
map_time

### THE END ###
