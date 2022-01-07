# Basics: Packages and Import ---------------------------------------------
imsbasics::clc()
library(tidyverse)
library(kml)
library(kmlShape)
library(GGally)
library(plm)
library(stargazer)
library(BBmisc)
library(REEMtree)
library(rattle)
library(knitr)

load("data/df_long.RData")

assertthat::assert_that(nrow(df_long) == 225296)
assertthat::assert_that(ncol(df_long) == 36)
# df_long <- df_long %>% select_if(~sum(!is.na(.)) > 0)


colnames(df_long) <- c("id",
                       "year",
                       "depression",
                       "arbeit_zeit_wochenstunden",
                       "arbeit_zeit_wochenstunden_vereinbart",
                       "arbeit_zeit_wochenende",
                       "arbeit_zeit_nacht",
                       "arbeit_zeit_art",
                       "arbeit_qualifikation",
                       "arbeit_intensitaet",
                       "arbeit_einbezug_entscheidungen",
                       "arbeit_zufriedenheit_atmosphaere",
                       "hausarbeit_wochenstunden",
                       "beeintraechtigung_arbeit_privat",
                       "abschalten_nach_arbeit",
                       "einschraenkung_weg_ges_zustand",
                       "tage_gesunheits_prob",
                       "chronische_krankheit",
                       "ausbildung",
                       "partnerschaft",
                       "tod_person",
                       "person_haushalt",
                       "migrationshintergrund",
                       "geschlecht",
                       "alter",
                       "status",
                       "occupa",
                       "ch_nationalitaet",
                       "haushaltsaequivalenzeinkommen",
                       "kinder_betreuung",
                       "pflege_extern",
                       "pflege_extern_wer1",
                       "pflege_extern_wer2",
                       "pflege_extern_wer3",
                       "pflege_extern_wer4",
                       "pflege_extern_wer5")


sample <- df_long %>%
  drop_na(alter) %>%
  filter(alter >= 15) %>%
  filter(alter <= 65) %>%
  filter(occupa %in% c(1,2,3,5,6))
rm(df_long)

sample <- mutate_all(sample, function(x) as.numeric(as.character(x)))

# Ueberstunden ------------------------------------------------------------
sample <- sample %>% mutate(
  arbeit_zeit_ueberstunden = arbeit_zeit_wochenstunden - arbeit_zeit_wochenstunden_vereinbart)

# Pflege von Angehoerigen -------------------------------------------------
sample <- sample %>% mutate(
  pflege_angehoerige = case_when(
    pflege_extern == 2 ~ 1,
    pflege_extern == 1 & !(pflege_extern_wer1 == id | pflege_extern_wer2 == id | pflege_extern_wer3 == id | pflege_extern_wer4 == id | pflege_extern_wer5 == id) ~ 1,
    pflege_extern == 1 & (pflege_extern_wer1 == id | pflege_extern_wer2 == id | pflege_extern_wer3 == id | pflege_extern_wer4 == id | pflege_extern_wer5 == id) ~ 2)
)
table(sample$pflege_angehoerige, useNA = "ifany")
sample$pflege_angehoerige <- factor(sample$pflege_angehoerige, levels = c(1,2), labels = c("Keine Pflege", "Pflege"))
table(sample$pflege_angehoerige, useNA = "ifany")


# depression über die jahre
sample %>% group_by(year) %>% summarise(mean(depression, na.rm = TRUE))
sample %>% mutate(alter = round(alter/10)) %>% group_by(alter) %>% summarise(mean(depression, na.rm = TRUE))

# einschraenkung_weg_ges_zustand über die jahre
sample %>% group_by(year) %>% summarise(mean(einschraenkung_weg_ges_zustand, na.rm = TRUE))
sample %>% mutate(alter = round(alter/10)) %>% group_by(alter) %>% summarise(mean(einschraenkung_weg_ges_zustand, na.rm = TRUE))

# arbeit_intensitaet über die jahre
sample %>% group_by(year) %>% summarise(mean(arbeit_intensitaet, na.rm = TRUE))
sample %>% mutate(alter = round(alter/10)) %>% group_by(alter) %>% summarise(mean(arbeit_intensitaet, na.rm = TRUE))




# Clustering functions ---------------------------------------------------------

extract_kml_clusters <- function(cluster, df_kml, type = "kml", j) {
  ### code to add clusters to the original data
  unique_id <- unique(df_kml$id)

  if (type == "kml") {
    id_not_na <- as.numeric(str_extract(cluster@idFewNA, "\\-*\\d+\\.*\\d*"))
    labels <- as.data.frame(cluster@c5[[1]]@clusters)
  } else if (type == "kmlShape") {
    id_not_na <- as.numeric(1:length(cluster@id))
    labels <- matrix()
    for(i in 1:length(cluster@clusters)){
      labels[[i]] <- (cluster@clusters[[i]])
    }
    labels <- as.data.frame(labels)
  }

  real_id_not_na <- list()
  for(i in 1:length(id_not_na)) {
    real_id_not_na[[i]] <- unique_id[id_not_na[i]]
  }

  real_id_not_na <- do.call(rbind.data.frame, real_id_not_na)
  df_labeled <- cbind(real_id_not_na, labels)
  colnames(df_labeled) <- c("id",paste0("cluster_",j))
  df_kml <- left_join(df_kml, df_labeled, by = "id")

  return(df_kml)
}



#' Run kml for 5 clusters (A,B,C,D,E) n times and do a re-assignment of clusters
#' A,B,C,D,E based on the mean of the mean-trajectories
#'
#' The re-assignment is error-prone because the mean of the identified mean-trajectories
#' does not account for their trend... BUT it worked for all trial-runs.
#'
#' @param df_kml
#' @param n
#' @param nbRedrawing
#'
#' @return
#' @export
#'
#' @examples
do_kml_clustering <- function(df_kml, n = 3, nbRedrawing = 1) {
  save_path <- paste0(getwd(),"/Scripts_KML/pics_kml/n=",n,"_nbRedrawing=",nbRedrawing,"/")
  if (!dir.exists(save_path)) {dir.create(save_path)}
  mean_traj_df <- data.frame()
  for (j in 1:n) {
    # create clustered data an then a ClusterLongData object.
    kml_cluster_data <- pivot_wider(df_kml[1:3], names_from = year, values_from = depression)
    cluster <- clusterLongData(as.matrix(kml_cluster_data[c(2:17)]))
    # cluster data
    kml(cluster, nbClusters = 5, nbRedrawing = nbRedrawing, toPlot='none',
        parAlgo = parALGO(saveFreq = Inf, # If we don't save we don't have problems
                                          # due tue environment-issues etc...
                                          #  -> when using save, use "cluster <<-"
                                          #     insteas of "cluster <-" above
                          startingCond = "nearlyAll")) # try different types of starting conditions.
                                                     #  - "randomAll"
                                                     #  - "randomK"
                                                     #  - "maxDist" -> gives always the same result
                                                     #  - "kmeans++"
                                                     #  - "kmeans+"
                                                     #  - "kmeans–"
                                                     #  - "kmeans–-"



    df_kml <- extract_kml_clusters(cluster, df_kml, "kml", j)

    # identify mean of mean clusters -> this gives our new order for cluster-names A,B,C,D,E
    # from which we re-assign the cluster in df_kml (by ordered mean-values).
    mean_clusters <- kml::calculTrajMean(traj = cluster@traj, clust = cluster@c5[[1]]@clusters)
    sort_order <- order(as_tibble(mean_clusters) %>% summarise(m = rowMeans(.[,])) %>% .$m)

    names_new <- levels(cluster@c5[[1]]@clusters)
    names_old <- names_new[sort_order]
    df_kml <- imsbasics::replace_by_lookuptable(df = df_kml, col = paste0("cluster_",j),
                                      lookup = data.frame(old = c(names_old, NA),
                                                          new = c(names_new, NA),
                                                          stringsAsFactors = FALSE))

    # plot the newly assiged clusters in the new order (the easiest way to plot
    # the new cluster is to run kml-again on the newly ordererd mean-trajectories.
    # this gives the same trajectories but in the new order)
    mean_clusters <- mean_clusters[sort_order,]
    kml_mean_clusters <- clusterLongData(mean_clusters)
    kml(kml_mean_clusters, nbClusters = 5, nbRedrawing = nbRedrawing, toPlot='none',
        parAlgo = parALGO(saveFreq = Inf, startingCond = "maxDist"))

    png(file=paste0(save_path,j,".png"), width = 1500, height = 600)
    par(mfrow = c(1,2))
    plot(cluster,5, toPlot = "traj")
    plot(kml_mean_clusters, 5, toPlot = "traj", ylim = c(0,10))
    dev.off()

    # add mean_cluster to mean_traj_df (to collect all mean-tranjectories)
    mean_clusters <- as.data.frame(mean_clusters)
    mean_clusters$cluster <- names_new
    mean_clusters$iteration <- j
    mean_traj_df <- rbind(mean_traj_df, mean_clusters)

  }
  # plot mean_traj_df = summary of all the (re-assigned) mean-trajectories.
  mean_traj_df <- mean_traj_df %>%
    pivot_longer(cols = starts_with("t"), names_to = "time",
                 names_prefix = "t", values_to = "depression")
  png(file=paste0(save_path, "summary.png"), width = 1000, height = 800)
  print(ggplot(mean_traj_df,
               aes(x = as.numeric(time), y = depression, group = interaction(cluster, iteration), col = cluster)) +
          geom_line())
  dev.off()

  png(file=paste0(save_path, "summary2.png"), width = 1000, height = 800)
  print(ggplot(mean_traj_df,
               aes(x = as.numeric(time), y = depression, group = interaction(cluster, iteration), col = cluster)) +
          geom_line() + facet_wrap(~cluster, nrow = 2))
  dev.off()

  imsbasics::save_rdata(df_kml, paste0("kml_clustering_",n,"_",nbRedrawing), save_path)
  return(df_kml)
}




#' Analogue to do_kml_clustering(), but for kmlShape
#'
#' @param df_kml
#' @param n
#'
#' @return
#' @export
#'
#' @examples
do_kmlShape_clustering <- function(df_kml, n = 3) {
  save_path <- paste0(getwd(),"/Scripts_KML/pics_kmlShape/n=",n,"/")
  if (!dir.exists(save_path)) {dir.create(save_path)}
  mean_traj_df <- data.frame()

  df_kml <- df_kml %>% drop_na() # drop NA entries (needed for kmlShape). It's important
  # to neglect ids that consist only of NA's

  for (j in 1:n) {
    kml_cluster_data <- pivot_wider(df_kml[1:3], names_from = year, values_from = depression)
    cluster <- cldsWide(data.frame(kml_cluster_data))
    reduceTraj(cluster, nbSenators = 100, imputationMethod = "linearInterpol")
    kmlShape(cluster, nbClusters = 5, timeScale = 0.1, FrechetSumOrMax = "max",
             toPlot="none", parAlgo = parKmlShape(aggregationMethod = "all",  maxIter = 500))

    df_kml <- extract_kml_clusters(cluster, df_kml, "kmlShape", j)

    # identify mean of mean clusters -> this gives our new order for cluster-names A,B,C,D,E
    # from which we re-assign the cluster in df_kml (by ordered mean-values).
    mean_clusters <- cluster@trajMeans
    sort_order <- order(mean_clusters %>% group_by(iCenters) %>% summarise(m = mean(traj)) %>% .$m)

    names_new <- levels(cluster@clusters)
    names_old <- names_new[sort_order]
    df_kml <- imsbasics::replace_by_lookuptable(df = df_kml, col = paste0("cluster_",j),
                                                lookup = data.frame(old = c(names_old, NA),
                                                                    new = c(names_new, NA),
                                                                    stringsAsFactors = FALSE))

    # plot the newly assiged clusters in the new order (the easiest way to plot
    # the mean trajectories with re-assigned clusters)
    mean_clusters <- imsbasics::replace_by_lookuptable(df = mean_clusters, col = "iCenters",
                                                       lookup = data.frame(old = c(names_old, NA),
                                                                           new = c(names_new, NA),
                                                                           stringsAsFactors = FALSE))
    image1 <- paste0(save_path,j,"-1.png")
    image2 <- paste0(save_path,j,"-2.png")
    png(file=image1, width = 750, height = 600)
    plot(cluster)
    dev.off()
    png(file=image2, width = 750, height = 750)
    print(ggplot(mean_clusters, aes(x = times, y = traj, col = factor(iCenters))) +
            geom_line(size = 2) + ylim(c(0,10)))
    dev.off()

    png(file=paste0(save_path,j,".png"), width = 3000, height = 1500)
    p1 <- png::readPNG(image1)
    p2 <- png::readPNG(image2)
    gridExtra::grid.arrange(grid::rasterGrob(p1, width = 1, height = 1),
                            grid::rasterGrob(p2, width = 1, height = 0.8),
                            nrow = 1)
    dev.off()
    file.remove(image1); file.remove(image2)

    # add mean_cluster to mean_traj_df (to collect all mean-tranjectories)
    mean_clusters$iteration <- j
    mean_traj_df <- rbind(mean_traj_df, mean_clusters)
  }
  # plot mean_traj_df = summary of all the (re-assigned) mean-trajectories.
  png(file=paste0(save_path, "summary.png"), width = 1000, height = 800)
  print(ggplot(mean_traj_df,
               aes(x = times, y = traj, group = interaction(iCenters, iteration), col = iCenters)) +
          geom_line())
  dev.off()

  png(file=paste0(save_path, "summary2.png"), width = 1000, height = 800)
  print(ggplot(mean_traj_df,
               aes(x = times, y = traj, group = interaction(iCenters, iteration), col = iCenters)) +
          geom_line() + facet_wrap(~iCenters, nrow = 2))
  dev.off()

  imsbasics::save_rdata(df_kml, paste0("kmlshape_clustering_",n), save_path)
  return(df_kml)
}



# ------------------------------- run functions  -------------------------------
# df_kml <- sample[1:3]
# set.seed(1)
# kml_clustering_10_1 <- do_kml_clustering(df_kml, n = 10, nbRedrawing = 1)
# kmlshape_clustering_10_1 <- do_kmlShape_clustering(df_kml, n = 10)
#


# ------------------------------- Analysis -------------------------------------
imsbasics::clc()
kml_clustering_10_1 <- imsbasics::load_rdata("kml_clustering_10_1", "Scripts_KML/pics_kml/n=10_nbRedrawing=1/")
kmlshape_clustering_10_1 <- imsbasics::load_rdata("kmlshape_clustering_10", "Scripts_KML/pics_kmlShape/n=10/")


my_df <- kml_clustering_10_1 # kmlshape_clustering_10_1


# We want to analyse the clusters per id
df1 <- my_df %>%
  select(-year, - depression) %>%
  distinct_all() %>% # take unique rows (since we don't have year and depression anymore)
  pivot_longer(cols = starts_with("cluster"), names_to = "iteration", names_prefix = "cluster_",
               values_to = "cluster") %>%
  mutate(iteration = as.numeric(iteration), cluster = factor(cluster))


# Assignment to clusters per ID (for first 1000 Entries)
ggplot(data = df1[1:2000,], aes(x = factor(id), y = 1, fill = cluster)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# The majority of id's stay within the same cluster. (We plot the maximal share,
# of a cluster)
n_clust <- max(df1$iteration)
shares <- df1 %>%
  filter(!is.na(cluster)) %>% # we only look at valid clusters A,B,C,D,E
  group_by(id, cluster) %>%
  summarise(share = n()/n_clust)
max_shares <- shares %>%
  filter(share == max(share)) %>%
  distinct(id, .keep_all = TRUE)
assertthat::assert_that(length(max_shares$id) == length(unique(max_shares$id)))

ggplot(max_shares, aes(x = share)) +
  geom_histogram() +
  labs(title = "Maximale Zugehörigkeit zu demselben Cluster",
       subtitle = "100%, wenn eine Id in allen Fällen demselben Cluster zugeteilt wurde")





