###functions.R

get_data <- function(num_clusters, points_per_cluster){
  
  # Generate random cluster centers
  centers_x <- runif(num_clusters, 0, 100)
  centers_y <- runif(num_clusters, 0, 100)
  
  # Generate points around each center
  x <- unlist(lapply(centers_x, function(c) rnorm(points_per_cluster, mean = c, sd = 5)))
  y <- unlist(lapply(centers_y, function(c) rnorm(points_per_cluster, mean = c, sd = 5)))
  
  df <- data.frame(x = x, y = y, cluster = rep(1:num_clusters, each = points_per_cluster))

return(df)
}

make_data_plot <- function(data){

  plot <- data %>%
    group_by(cluster) %>%
    mutate(centroid_x = mean(x),
           centroid_y = mean(y)) %>%
    mutate(cluster = factor(cluster)) %>%
    ggplot()+
    geom_point(aes(x = x, y = y, color = cluster))+
    geom_point(aes(x = centroid_x, y = centroid_y), color = "red",size = 2, shape = 17)+
    scale_color_jama()+
    theme_minimal() +
    labs(fill = "Cluster")

return(plot)
}

make_grid_data <- function(data){
  

  df_sf <- data %>%
    st_as_sf(coords = c("x", "y"))

  
  grid <- df_sf %>%
    st_make_grid(cellsize = 5) %>%
    st_as_sf() %>% 
    mutate(ID = 1:length(x))%>%
    st_join(df_sf) %>%
    group_by(ID) %>%
    summarise(n = n())

  
  grid_plot <- grid %>%
  ggplot()+
    geom_sf(aes(fill = n), lwd = .1, color = "lightgrey") +
    scale_fill_gradient(low = "white",
                        high = "red") +
    theme_minimal() +
    labs(fill = "Count")

  return(list(grid, grid_plot))
}

make_moran <- function(data){

  nb <- poly2nb(data)
  listw <- nb2listw(nb, style = "W")
  
  lisa <- localmoran(data$n, listw)
  
  df_lisa <- cbind(data, lisa)
  df_lisa$cat <-   unlist(attr(lisa, "quadr")[1])
  df_lisa$plot_cat <- if_else(df_lisa$Pr.z....E.Ii.. > 0.05, NA,
                              df_lisa$cat)

all_plot <- df_lisa%>%
  ggplot()+
  geom_sf(aes(fill = cat),color = "lightgrey")+
  theme_minimal()+
  scale_fill_manual(values = c("blue",
                     "pink",
                     "lightblue",
                     "red"))

sig_plot <- df_lisa%>%
  ggplot()+
  geom_sf(aes(fill = plot_cat),color = "lightgrey")+
  theme_minimal()+
  scale_fill_manual(values = c("lightblue",
                               "red"), na.value = "grey") +
  labs(fill = "Local moran's I")

return(list(all_plot, sig_plot))
}

make_geary <- function(data){
  nb <- poly2nb(data)
  listw <- nb2listw(nb, style = "W")
  
  lg <- localG(data$n, listw)

  df_lg <- cbind(data, lg)

    df_lg$cat <-   unlist(attr(lg, "cluster"))

  localg<- df_lg %>%
  ggplot()+
    geom_sf(aes(fill = cat))+
    scale_fill_manual(values = c("blue", "red"))+
    labs(fill = "Local G*")
  
  localg_sig<- df_lg %>%
    mutate(cat = if_else(lg > -1.96 & lg < 1.96, NA, cat)) %>%
    ggplot()+
    geom_sf(aes(fill = cat))+
    scale_fill_manual(values = c("blue", "red"))+
    labs(fill = "Local G*")
  localg_sig
  
  return(localg)
  }

