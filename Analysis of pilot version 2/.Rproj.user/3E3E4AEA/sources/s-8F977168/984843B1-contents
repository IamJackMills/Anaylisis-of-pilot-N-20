#loading Libraries

library(plyr)
library(readr)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(factoextra)
library(nandb)


#loading all csvs and putting them all into a data frame with the catch questions data

file_list <- list.files(path = "CSV Responses", pattern = "*.csv", full.names = T)

getting_the_data_ready <- function(csv_document){
  raw_data <- read_csv(csv_document)
  wrangled_csv <- data.frame(catch_response = raw_data$catchresponse,
                             catch_question= raw_data$catchnumber,
                             catch_order = raw_data$catchtrialorder,
                             participant_id = raw_data$participant) %>% 
    .[-c(1:6),] 
}

dataframe_list <- map(file_list, getting_the_data_ready)

#checking to see each catch score

participant_correctcatch = list()
correctcatch = c()
dataframe_list[[12]]
for (i in 1:19){
  catch_order_this_trial <- (dataframe_list[[i]]$catch_order[1]) #loads the catch order in as a string of a python array. The following 4 lines converts it into an r list of intigers
  catch_order_this_trial <- gsub('[[]', '', x =catch_order_this_trial) # replacing '[' with an empty string
  catch_order_this_trial <- gsub('[]]', '', x =catch_order_this_trial) # replacing ']' with an empty string
  catch_order_this_trial <- as.list(strsplit(catch_order_this_trial, ',')[[1]]) # splitting the string on every comma
  catch_order_this_trial <- strtoi(c(catch_order_this_trial)) #converts string to integer
  catch_rate_data <- data.frame(catch_response = dataframe_list[[i]]$catch_response, #creates a table with the catch data
                                catch_question= dataframe_list[[i]]$catch_question) %>%
    .[catch_order_this_trial,]
  correct_catch <- 0
  for (j in 1:length(catch_rate_data$catch_question)){
    if (is.na(catch_rate_data$catch_response[j])){
      catch_rate_data$catch_response[j] = 9 
    }
    #I set empty cells to 9 because some of the responses had NA values for some reason and this was creating an error when doing the comparrison.
    if (catch_rate_data$catch_response[j] == catch_rate_data$catch_question[j]){
      correct_catch <- correct_catch+1
    }
    #Correct_catch is the number of catch questions answered correctly by participant i
  }
  participant_correctcatch[[i]] <-c(i,correct_catch,dataframe_list[[i]]$participant_id[1]) 
  #participant_correct catch has the number of correct answers to the catch questions and the participants id
}
correct_catch_vector <- unlist(map(participant_correctcatch ,\(x){as.numeric(x[2])/10}))

histogram <- hist(correct_catch_vector, xlab="correct catch%", breaks =c(.05,.15,.25,.35,.45,.55,.65,.75,.85,.95,1.05))
correct_catch_vector

histogram <- ggplot(data = data.frame(pctcorrect=correct_catch_vector), aes(x=pctcorrect))+
  geom_histogram()


#I made a new folder titled "Participant_CSV_responses_accepted" which has only participants with more than 75% correct catch response
#with this folder I load  all the accepted csvs into one large csv and a dataframe.

file_list_accepted <- list.files(path = "CSV Responses All", pattern = "*.csv", full.names = T)
file_list_accepted_v1 <- list.files(path = "CSV Responses All V1", pattern = "*.csv", full.names = T)

getting_the_data_ready <- function(csv_document){
  raw_data <- read_csv(csv_document)
  wrangled_csv <- data.frame("similarity" = raw_data$similarity,
                             image1 = raw_data$image1,
                             image2= raw_data$image2,
                             response_time = raw_data$response_time,
                             catch_response = raw_data$catchresponse,
                             catch_question= raw_data$catchnumber,
                             catch_order = raw_data$catchtrialorder,
                             this_rep = raw_data$trials_2.thisRepN) %>% 
    mutate(similarity_uniform = similarity -0.5*sign(similarity)) %>% .[-c(1:6),] 
}

file_list_accepted

combined_csv <- ldply(.data = file_list_accepted, .fun = getting_the_data_ready)
dataframe_list <- map(file_list_accepted, getting_the_data_ready)
dataframe_listv1 <- map(file_list_accepted_v1, getting_the_data_ready)


#creating a histogram.
histogram <- ggplot(combined_csv, aes(x=similarity))+
  stat_bin(colour = 'white', breaks = c(seq(-4.5,4.5,1)))+
  xlab("Mean Similarity")+
  ylab("Count")

combined_csv_mean <- combined_csv %>% 
  group_by(image1, image2) %>% 
  summarise(mean_similarity = mean(similarity_uniform),
            count = length(similarity_uniform),
            variance = var(similarity_uniform),
            standard_error_of_mean = sqrt(variance/count))

#By storing the images as factors instead of strings I can sort them in the order of the vector bellow. This arrangement makes the following raster plots look nice
order = c('Red', 'Yellow', 'LightGreen', 'DarkGreen', 'DarkBlue', 'Turquiose', 'LightBlue', 'Purple', 'Pink')
combined_csv_mean$image1 <- as.factor(combined_csv_mean$image1)
combined_csv_mean$image2 <- as.factor(combined_csv_mean$image2)
combined_csv_mean$image2 <- factor(combined_csv_mean$image2 ,levels = order)
combined_csv_mean$image1 <- factor(combined_csv_mean$image1 ,levels = order)

#plotting the mean in a raster plot
mean_raster_plot <- ggplot(combined_csv_mean, aes(x = image2, y = image1))+
  geom_raster(aes(fill = mean_similarity)) +
  scale_fill_gradientn(colours = c("black","white")) +
  guides(fill=guide_legend(title="Similarity")) +
  theme(
    axis.text.x = element_text(angle = 90)
  )+
  xlab('Right Image')+
  ylab('Left Image')

#plotting the variance in a raster plot
variance_colours <- rev(brewer.pal(9, 'RdYlGn'))

variance_raster_plot <- ggplot(combined_csv_mean, aes(x = image2, y = image1)) +
  geom_raster(mapping = aes(fill = variance)) +
  scale_fill_gradientn(colours = variance_colours, limits = c(0,6)) +
  guides(fill=guide_legend(title="Variance"))+
  ggtitle("Variance")+
  theme(
    axis.text.x = element_text(angle = 90)
  )+
  xlab('Right Image')+
  ylab('Left Image')



#plotting the variance of each comparison in a box graph
variance_box <- ggplot(combined_csv_mean, aes(y = variance, x = mean_similarity))+
  geom_boxplot()+
  geom_point()+
  facet_wrap(~image1)+
  theme(axis.text.x = element_blank())

variance_box2 <- ggplot(combined_csv_mean, aes(y = variance, x = 0))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~image2)+
  theme(axis.text.x = element_blank())

mean_box<- ggplot(combined_csv_mean, aes(y = mean_similarity, x = 0))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~image1)+
  theme(axis.text.x = element_blank())


#comparing the similarity when the images swap sides


combined_csv_mean_temp<- data.frame(mean_similarity= combined_csv_mean$mean_similarity,
                                    image1 = combined_csv_mean$image1,
                                    image2 = combined_csv_mean$image2)

similarity_table <- combined_csv_mean_temp %>% 
  pivot_wider(names_from = image1,
              values_from = mean_similarity,
              names_sort = TRUE) %>% 
  arrange(image2)

difference_table <- similarity_table

similarity_table <- similarity_table %>% column_to_rownames('image2')

#this makes difference table into a matrix that has the absolute value of the difference between the left and right comparisions
for (i in 1:9) {
  for (j in 1:9) {
    difference_table[i,j+1] = abs(similarity_table[i,j] - similarity_table[j,i])
  }
}

difference_in_rating_across_eyes <- difference_table %>% 
  pivot_longer(cols = all_of(order)) %>%
  mutate(image1 = name) %>% 
  select(-name)


#reordering the differences across eyes to make the raster plot look nicer and removing the lower triangle becuase its symetrical

difference_in_rating_across_eyes$image1 <- as.factor(difference_in_rating_across_eyes$image1)
difference_in_rating_across_eyes$image2 <- as.factor(difference_in_rating_across_eyes$image2)
difference_in_rating_across_eyes$image2 <- factor(difference_in_rating_across_eyes$image2 ,levels = order)
difference_in_rating_across_eyes$image1 <- factor(difference_in_rating_across_eyes$image1 ,levels = order)

#the following block of code assigns a number to each image colour (ie red = 1, yellow = 2) 
#and then removes all pairs where the number associated with image 1 is less than the number with image 2. 
#this means that each pair only appears once
difference_in_rating_across_eyes <- difference_in_rating_across_eyes %>% 
  mutate(image1number = as.numeric(image1),image2number = as.numeric(image2)) %>% 
  filter(as.integer(image1number)>=as.integer(image2number)) %>% 
  select(-c(image1number,image2number))

#plotting the differences in a raster plot
plot2 <- ggplot(difference_in_rating_across_eyes, aes(x = image2, y = image1))+
  geom_raster(aes(fill = value)) +
  scale_fill_gradientn(colours = c("white","black")) +
  guides(fill=guide_legend(title="Difference")) +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.major = element_blank()
  )

dataframe_list_ungrouped<-dataframe_list

#plotting individual raster plots
for (i in 1:20){
  dataframe_list[[i]]<- dataframe_list[[i]] %>% 
    group_by(image1, image2) %>% 
    summarise(mean_similarity = mean(similarity_uniform),
              variance_ = var(similarity_uniform),
              index = i)
}

for (i in 1:20){
  dataframe_listv1[[i]]<- dataframe_listv1[[i]] %>% 
    group_by(image1, image2) %>% 
    summarise(mean_similarity = mean(similarity_uniform),
              variance_ = var(similarity_uniform),
              index = i+20)
}

for (i in 1:20){
  individual_similarity <- dataframe_list[[i]]
  
  index = as.character(i)
  
  individual_similarity$image1 <- as.factor(individual_similarity$image1)
  individual_similarity$image2 <- as.factor(individual_similarity$image2)
  individual_similarity$image2 <- factor(individual_similarity$image2 ,levels = order)
  individual_similarity$image1 <- factor(individual_similarity$image1 ,levels = order)
  
  catch_correct_this_trial <- paste(as.character(correct_catch_vector[i]*100), '%', as.character(index), 'Mean')
  
  individual_similarity_plot <- ggplot(individual_similarity, aes(x = image2, y = image1)) +
    geom_raster(mapping = aes(fill = mean_similarity)) +
    scale_fill_gradient(low = 'black', high = 'white', limits = c(-4,4)) +
    ggtitle(catch_correct_this_trial) +
    guides(fill=guide_legend(title="Similarity"))+
    theme(
      axis.text.x = element_text(angle = 90), 
      plot.title = element_text(hjust = -0.4)
    )+
    xlab('Right Image')+
    ylab('Left Image')
  
  individual_variance <- dataframe_list[[i]]
  
  individual_variance$image1 <- as.factor(individual_variance$image1)
  individual_variance$image2 <- as.factor(individual_variance$image2)
  individual_variance$image2 <- factor(individual_variance$image2 ,levels = order)
  individual_variance$image1 <- factor(individual_variance$image1 ,levels = order)
  
  variance_colours <- rev(brewer.pal(9, 'RdYlGn'))
  
  individual_variance_plot <- ggplot(individual_variance, aes(x = image2, y = image1)) +
    geom_raster(mapping = aes(fill = variance_)) +
    scale_fill_gradientn(colours = variance_colours, limits = c(0,25)) +
    guides(fill=guide_legend(title="Variance"))+
    ggtitle("Variance")+
    theme(
      axis.text.x = element_text(angle = 90)
    )+
    xlab('Right Image')+
    ylab('Left Image')
  
  plot_combined <- ggarrange(individual_similarity_plot,individual_variance_plot )
  
  
  name = paste('individual_raster_plot', index, '.jpeg')
  ggsave(filename = name, plot = plot_combined, device = "jpeg", width = 2450, height = 1170, units='px')
}

#plotting answer time against score
median_response_time <- data.frame(response_time = combined_csv$response_time,
                                   similarity = combined_csv$similarity) %>% 
  group_by(similarity) %>% 
  summarise(median_time = median(response_time))


median_response_dots <- ggplot(data = median_response_time, mapping = aes(x = similarity, y = median_time)) +
  geom_point()+
  ylim(0,2)

median_response_time_by_colour <- data.frame(response_time = combined_csv$response_time,
                                             image1 = combined_csv$image1,
                                             image2 = combined_csv$image2) %>% 
  group_by(image1, image2) %>% 
  summarise(median_time = median(response_time))

#sorting them to make the raster plot look nice
median_response_time_by_colour$image1 <- as.factor(median_response_time_by_colour$image1)
median_response_time_by_colour$image2 <- as.factor(median_response_time_by_colour$image2)
median_response_time_by_colour$image2 <- factor(median_response_time_by_colour$image2 ,levels = order)
median_response_time_by_colour$image1 <- factor(median_response_time_by_colour$image1 ,levels = order)

median_response_raster <- ggplot(data = median_response_time_by_colour,
                                 mapping = aes(x = image1, y = image2)) +
  geom_raster(aes(fill = median_time))+
  scale_fill_gradientn(colours = c("white","black")) +
  guides(fill=guide_legend(title="Response Time")) +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.major = element_blank())

#comparing the when the images swap sides 
#since all the groups were the same size we don't need to calculate each mean before the ttest.
p_value_table <- similarity_table

for (j in 1:9){
  for (k in 1:9){
    if (j==k){
      p_value_table[j,k] <- 1
      next
    }
    temp1<-c()
    temp2<-c()
    for (i in 1:length(combined_csv$similarity)){
      if (combined_csv$image1[i]==order[j] & combined_csv$image2[i]==order[k]){
        temp1 <- append(temp1, combined_csv$similarity_uniform[i])
      }
      if (combined_csv$image2[i]==order[j] & combined_csv$image1[i]==order[k]){
        temp2 <- c(temp2,combined_csv$similarity_uniform[i])
      }
    }
    p_value_table[j,k] <-t.test(temp1,temp2)$p.value
    
  }
}
p_value_table1 <- p_value_table


#None of the differences are statistically significant so I think it means we can symmeterise the data

#symmeterising the data. 
#to symeterise the data, we can just add the table to itself but swap the images when we add them.
#when doing this we have to make sure not to double up on the comparisons of the same images especially when looking at variance
#the following chunk creates a dataframe with no comparisons of the same colour
rows_with_the_same_image <- c()
for (i in 1:length(combined_csv$image1)){
  if (combined_csv$image1[i] == combined_csv$image2[i]){
    rows_with_the_same_image <- c(rows_with_the_same_image,i)
  }
}
combined_csv_no_doubles <- combined_csv[-rows_with_the_same_image,]

combined_csv_symmeterised <- tibble(image1 = c(combined_csv$image1,combined_csv_no_doubles$image2),
                                    image2 = c(combined_csv$image2,combined_csv_no_doubles$image1),
                                    similarity_uniform = c(combined_csv$similarity_uniform, combined_csv_no_doubles$similarity_uniform))

combined_csv_symmeterised$image1 <- as.factor(combined_csv_symmeterised$image1)
combined_csv_symmeterised$image2 <- as.factor(combined_csv_symmeterised$image2)
combined_csv_symmeterised$image2 <- factor(combined_csv_symmeterised$image2 ,levels = order)
combined_csv_symmeterised$image1 <- factor(combined_csv_symmeterised$image1 ,levels = order)

combined_csv_symmeterised_mean <- combined_csv_symmeterised %>% group_by(image1,image2) %>% 
  summarise(mean_similarity = mean(similarity_uniform),
            variance = var(similarity_uniform),
            count = length(similarity_uniform))



#raster plots of the symeterised data.
mean_raster_plot_symeterised <- ggplot(combined_csv_symmeterised_mean, aes(x = image2, y = image1))+
  geom_raster(aes(fill = mean_similarity)) +
  scale_fill_gradientn(colours = c("black","white")) +
  guides(fill=guide_legend(title="Similarity")) +
  theme(
    axis.text.x = element_text(angle = 90)
  )+
  xlab('Right Image')+
  ylab('Left Image')


variance_raster_plot_symeterised <- ggplot(combined_csv_symmeterised_mean, aes(x = image2, y = image1)) +
  geom_raster(mapping = aes(fill = variance)) +
  scale_fill_gradientn(colours = variance_colours, limits = c(0,6)) +
  guides(fill=guide_legend(title="Variance"))+
  ggtitle("Variance")+
  theme(
    axis.text.x = element_text(angle = 90)
  )+
  xlab('Right Image')+
  ylab('Left Image')

#box plots of the symeterised data
mean_box <- ggplot(combined_csv_symmeterised_mean, aes(y = mean_similarity, x = 0))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~image1)+
  theme(axis.text.x = element_blank())

colours_vector3 <- c("red", "yellow","lawngreen","darkgreen", "darkblue","turquoise1","dodgerblue","darkorchid","pink")

variance_box <- ggplot(combined_csv_symmeterised_mean, aes(y = variance))+
  geom_boxplot()+
  geom_point(aes(x=mean_similarity, col=image2, fill=image2))+
  facet_wrap(~image1)+
  scale_colour_manual(values = colours_vector3)+
  theme(legend.position = 'none')+
  xlab("Similarity")


colours_vector2
#variance vs mean plot


variance_vs_mean <- ggplot(data = combined_csv_symmeterised_mean, mapping = aes(x=mean_similarity, y= variance))+
  geom_point()

#creating a cmd plot of the data
combined_csv_symmeterised_temp<- tibble(image1 = combined_csv_symmeterised$image1,
                                        image2 = combined_csv_symmeterised$image2,
                                        mean_similarity = -(combined_csv_symmeterised$similarity_uniform-3.5))

similarity_table_symeterised <- combined_csv_symmeterised_temp %>% 
  group_by(image1, image2) %>% 
  summarise(mean_similarity = mean(mean_similarity)) %>% 
  pivot_wider(names_from = image1,
              values_from = mean_similarity,
              names_sort = TRUE) %>% 
  arrange(image2) %>% 
  column_to_rownames('image2')


distances <- as.data.frame(cmdscale(as.dist(similarity_table_symeterised), k=2)) %>% 
  rownames_to_column(var="colour") 

distances$colour <- as.factor(distances$colour)
distances$colour <- factor(distances$colour ,levels = order)


colours_vector <- c("#2d3038", "#4c5b33", "#818695", "#8e9a56", "#8e9a56", "7f6e8c", "#612436", "#446c64", "#ce885a")
distances$cols <-  colours_vector3

cmd_plot <- ggplot(data = distances, aes(x=V1,y=V2, col = colour, fill = colour))+
  geom_point(aes(size=5))+
  scale_colour_manual(values = colours_vector3)+
  theme(legend.position = 'none')


#In subject consistency from trail 1 to trail 2.
#to make the plots we need put trail one on the x axis and trail 2 on the y axis
#I think the easiest way to do this is to make two lists, one with all the ratings for trail 1
#and the other for the ratings with trial two
#and their indexes are matching(ie the first entry in list one is the ranking for the same image as the first entry for list two)
#the way I have got the list could be easier but some other analysis I did earlier messed up the order of the entries.




temp3 <- c()
temp4 <- c()
cor_vector <- c() #this vector will store all the correlation coefficients 
for (individual in 1:20){ #this loop will iterate through the twenty participants
  temp1<-c()#temp 1 will store the rankings of the first trial
  temp2<-c()#temp 2 will store the rankings of the second trial
  l = 1 
  for (i in order){#this iterates through the left image
    for (j in order){#this iterates through the right image
      for (k in c(1:162)){#this iterates through every row
        if (dataframe_list_ungrouped[[individual]]$image1[k] == i & dataframe_list_ungrouped[[individual]]$image2[k] == j){#does the current row compare the correct images?
          if (l == 1){#is the current compaison of the correct two images the first or the second appearance of it in this individual
            temp1 <- c(temp1,dataframe_list_ungrouped[[individual]]$similarity_uniform[k])#if it is the first then add it to trial one
            l <- 2
          }
          else{
            temp2 <- c(temp2,dataframe_list_ungrouped[[individual]]$similarity_uniform[k])#if it is the second the add it to trial two
            l <- 1
          }
        }
      }
    }
  }
  temp3 <- c(temp3, temp1)
  temp4 <- c(temp4, temp2)
  
  temp_df <- tibble(trail1 = temp1, trail2 = temp2)#creates a tabble from the two vectors
  corilation <-  paste(as.character(individual), "ρ =",as.character(cor(x = temp1, y = temp2)))#is the title for the image
  plot4 <- ggplot(data =temp_df, mapping = aes(x=trail1, y =trail2))+#this plots the image
    geom_jitter()+#geom_jitter is used instead of geom_point to see density
    ggtitle(corilation)
  cor_vector <- c(cor_vector,cor(x = temp1, y = temp2))#adds the corilation coefficent to the vector
  name <- paste("corrilatition plot", as.character(individual),".jpeg")#the name that the image will save to
  ggsave(filename = name, plot = plot4, device = "jpeg")#saves an image of the plot(maybe comment this out to avoid saving 20 images each time you run the loop)
}

dist_cor <- tibble(dist = cor_vector)
histogram <- ggplot(data = dist_cor, aes(x=dist))+
  geom_histogram(bins = 5)+
  xlab("Within Individual Trial Correlation")

mean(cor_vector)

#overall consistency from trial 1 to trial two.

combined_csv_by_trial <- combined_csv %>% 
  group_by(this_rep,
           image1,
           image2) %>% 
  summarise(similarity_mean = mean(similarity_uniform))


l <- 1
temp1<-c()
temp2 <- c()
for (i in order){
  for (j in order){
    for (k in c(1:162)){
      if (combined_csv_by_trial$image1[k] == i & combined_csv_by_trial$image2[k] == j){
        if (l == 1){
          temp1 <- c(temp1,combined_csv_by_trial$similarity_mean[k])
          l <- 2
        }
        else{
          temp2 <- c(temp2,combined_csv_by_trial$similarity_mean[k])
          l <- 1
        }
      }
    }
  }
}

temp_df3 <- tibble(trail1 = temp1, trail2 = temp2)
corilation <-  paste("Across all participants", "ρ =",as.character(cor(x = temp1, y = temp2)))
plot4 <- ggplot(data =temp_df3, mapping = aes(x=trail1, y =trail2))+
  geom_jitter()+
  ggtitle(corilation)
cor(x = temp1, y = temp2)


temp_df2 <- tibble(trail1 = temp3, trail2 = temp4)
corilation <-  paste("Across all participants", "ρ =",as.character(cor(x = temp3, y = temp4)))
plot5 <- ggplot(data =temp_df2, mapping = aes(x=trail1, y =trail2))+
  geom_jitter()+
  ggtitle(corilation)
cor(x = temp1, y = temp2)
#Making a covaraiance matrix of the 20 people.
image1<-c()
image2<-c()

for (i in order){#this iterates through the left image
  for (j in order){#this iterates through the right image
    image1 <- c(image1, i)
    image2 <- c(image2, j)
  }
}

#creating r_ij covariance matricies.

dataframe_list_combined <- c(dataframe_listv1 , dataframe_list)

table_of_comparisons_combined<-dataframe_list_combined %>% 
  bind_rows() %>%
  ungroup() %>%
  select(image1, image2, index,mean_similarity) %>%  
  pivot_wider(names_from = index, values_from = mean_similarity)

table_of_comparisonsv1<-dataframe_listv1 %>% 
  bind_rows() %>%
  ungroup() %>%
  select(image1, image2, index,mean_similarity) %>%  
  pivot_wider(names_from = index, values_from = mean_similarity)

table_of_comparisons<-dataframe_list%>% 
  bind_rows() %>%
  ungroup() %>%
  select(image1, image2, index,mean_similarity) %>%  
  pivot_wider(names_from = index, values_from = mean_similarity)

table_of_comparisons_combined <- table_of_comparisons_combined %>% 
  mutate(image = paste(image1,image2)) %>% 
  select(-1,-2) %>% 
  column_to_rownames('image')


table_of_comparisonsv1 <- table_of_comparisonsv1 %>% 
  mutate(image = paste(image1,image2)) %>% 
  select(-1,-2) %>% 
  column_to_rownames('image')

table_of_comparisons <- table_of_comparisons %>% 
  mutate(image = paste(image1,image2)) %>% 
  select(-1,-2) %>% 
  column_to_rownames('image')

covariance_matrix_combined <- cor(table_of_comparisons_combined)
covariance_matrixv1 <- cor(table_of_comparisonsv1)
covariance_matrix <- cor(table_of_comparisons)

mean(covariance_matrixv1)
mean(covariance_matrix)
matrix_raster_plot(
  as.matrix(covariance_matrix_combined),
  scale_name = "scale",
  limits = NULL,
  ranges = NULL,
  range_names = NULL,
  colours = NULL,
  na_colour = "black",
  clip = FALSE,
  clip_low = FALSE,
  clip_high = FALSE,
  log_trans = FALSE,
  breaks = NULL,
  include_breaks = NULL
)+
  ggtitle(paste("Overall Cor Mean =", mean(covariance_matrix_combined)))
  
 matrix_raster_plot(
  as.matrix(covariance_matrixv1),
  scale_name = "scale",
  limits = NULL,
  ranges = NULL,
  range_names = NULL,
  colours = NULL,
  na_colour = "black",
  clip = FALSE,
  clip_low = FALSE,
  clip_high = FALSE,
  log_trans = FALSE,
  breaks = NULL,
  include_breaks = NULL
)+
  ggtitle(paste("Version 1 Cor Mean =",mean(covariance_matrixv1)))

matrix_raster_plot(
  as.matrix(covariance_matrix),
  scale_name = "scale",
  limits = NULL,
  ranges = NULL,
  range_names = NULL,
  colours = NULL,
  na_colour = "black",
  clip = FALSE,
  clip_low = FALSE,
  clip_high = FALSE,
  log_trans = FALSE,
  breaks = NULL,
  include_breaks = NULL
)+
  ggtitle(paste("Version 2 Cor Mean =",mean(covariance_matrix)))
