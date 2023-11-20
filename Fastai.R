
#==========Fastai==========

# Install miniconda and activate environment:

reticulate::install_miniconda()
reticulate::conda_create('r-reticulate')

# The dev version:
devtools::install_github('eagerai/fastai')

# Later, you need to install the python module fastai:

fastai::install_fastai(gpu = FALSE, cuda_version = '10.1', overwrite = FALSE)

# Restart RStudio!


#=======================Tabular data=======================

library(magrittr)
library(fastai)

# download
URLs_ADULT_SAMPLE()

# read data
df = data.table::fread('adult_sample/adult.csv')


# Variables:

dep_var = 'salary'
cat_names = c('workclass', 'education', 'marital-status', 'occupation', 'relationship', 'race')
cont_names = c('age', 'fnlwgt', 'education-num')

# Preprocess strategy:

procs = list(FillMissing(),Categorify(),Normalize())


# Prepare:


dls = TabularDataTable(df, procs, cat_names, cont_names,
                       y_names = dep_var, splits = list(c(1:32000),c(32001:32561))) %>%
  dataloaders(bs = 64)


# Summary:

model = dls %>% tabular_learner(layers=c(200,100), metrics=accuracy)
model %>% summary()


# Before fitting try to find optimal learning rate:

model %>% lr_find()

model %>% plot_lr_find(dpi = 200)


# Run:

model %>% fit(5, lr = 10^-1)

# Plot loss history:

model %>% plot_loss(dpi = 200)

# At the same time, users can find optimal batch size.

bss = model %>% bs_find(lr=1e-3)

model %>% plot_bs_find()

# Get confusion matrix:

model %>% get_confusion_matrix()

# Plot it:

interp = ClassificationInterpretation_from_learner(model)

interp %>% plot_confusion_matrix(dpi = 90,figsize = c(6,6))

# Get predictions on new data:

model %>% predict(df[10:15,])

# First, get explanation object:

exp = ShapInterpretation(model,n_samples = 20)

# Then, visualize decision plot:

exp %>% decision_plot(class_id = 1, row_idx = 2)

# Dependence plot:

exp %>% dependence_plot('age', class_id = 0)

# Summary plot:

exp %>% summary_plot()

# Waterfall plot:

exp %>% waterfall_plot(row_idx=10)

# Force (JS) plot:

exp %>% force_plot(class_id = 0)


#=======================Image data=======================

# Get Pets dataset:

URLs_PETS()

# Define path to folders:

path = 'oxford-iiit-pet'
path_anno = 'oxford-iiit-pet/annotations'
path_img = 'oxford-iiit-pet/images'
fnames = get_image_files(path_img)


# See one of examples:

fnames[1]

oxford-iiit-pet/images/american_pit_bull_terrier_129.jpg

# Dataloader:

dls = ImageDataLoaders_from_name_re(
  path, fnames, pat='(.+)_\\d+.jpg$',
  item_tfms=Resize(size = 460), bs = 10,
  batch_tfms=list(aug_transforms(size = 224, min_scale = 0.75),
                  Normalize_from_stats( imagenet_stats() )
  )
)



# Show batch for visualization:
  
dls %>% show_batch(dpi = 150)

# Model architecture:
  
learn = cnn_learner(dls, resnet34(), metrics = error_rate)


# And fit:

learn %>% fit_one_cycle(n_epoch = 2)

# Get confusion matrix and plot:
  
conf = learn %>% get_confusion_matrix()

library(highcharter)

hchart(conf, label = TRUE) %>%
  hc_yAxis(title = list(text = 'Actual')) %>%
  hc_xAxis(title = list(text = 'Predicted'),
           labels = list(rotation = -90))

# Plot top losses:

interp = ClassificationInterpretation_from_learner(learn)

interp %>% plot_top_losses(k = 9, figsize = c(15,11))


# Alternatively, load images from folders:


# get sample data
URLs_MNIST_SAMPLE()

# transformations
tfms = aug_transforms(do_flip = FALSE)
path = 'mnist_sample'
bs = 20

#load into memory
data = ImageDataLoaders_from_folder(path, batch_tfms = tfms, size = 26, bs = bs)

# Visualize and train
data %>% show_batch(dpi = 150)

learn = cnn_learner(data, resnet18(), metrics = accuracy)
learn %>% fit(2)


# There is a function in fastai timm_learner which originally written by Zachary Mueller.
# It helps to quickly load the pretrained models from timm library.


# First, lets's see the list of available models (TOP 10):
str(as.list(timm_list_models()[1:10]))

# Exciting!
 
# Now, load and train pets dataset:

library(magrittr)
library(fastai)

path = 'oxford-iiit-pet'

path_img = 'oxford-iiit-pet/images'

fnames = get_image_files(path_img)

dls = ImageDataLoaders_from_name_re(
  path, fnames, pat='(.+)_\\d+.jpg$',
  item_tfms=Resize(size = 460), bs = 10,
  batch_tfms=list(aug_transforms(size = 224, min_scale = 0.75),
                  Normalize_from_stats( imagenet_stats() )
  )
)

learn = timm_learner(dls, 'cspdarknet53', metrics = list(accuracy, error_rate))

learn %>% summary()


# And finally, fit:

learn %>% fit_one_cycle(3)

# See results:

learn %>% show_results()




#=======================Collab (Collaborative filtering)=======================

# Call libraries:
  
library(zeallot)
library(magrittr)

# Get data:
  
URLs_MOVIE_LENS_ML_100k()

# Specify column names:

c(user,item,title)  %<-% list('userId','movieId','title')

# Read datasets:

ratings = fread('ml-100k/u.data', col.names = c(user,item,'rating','timestamp'))
movies = fread('ml-100k/u.item', col.names = c(item, 'title', 'date', 'N', 'url',
                                               paste('g',1:19,sep = '')))
  
  
# Left join on item:

rating_movie = ratings[movies[, .SD, .SDcols=c(item,title)], on = item]

# Load data from dataframe (R):
  
dls = CollabDataLoaders_from_df(rating_movie,
                                seed=42,
                                valid_pct=0.1,
                                bs=64,
                                item_name=title,
                                path='ml-100k')

  
# Build model:

learn = collab_learner(dls, n_factors = 40, y_range=c(0, 5.5))

# Start learning:

learn %>% fit_one_cycle(1, 5e-3,  wd = 1e-1)

# Get top 1,000 movies:
  
top_movies = head(unique(rating_movie[ , count := .N, by = .(title)]
                         [order(count,decreasing = T)]
                         [, c('title','count')]),
                  1e3)[['title']]
  
# Find mean ratings for the films:
  
mean_ratings = unique(rating_movie[ , .(mean = mean(rating)), by = title])


# Extract bias:

movie_bias = learn %>% get_bias(top_movies, is_item = TRUE)

result = data.table(bias = movie_bias,
                    title = top_movies)

res = merge(result, mean_ratings, all.y = FALSE)

res[order(bias, decreasing = TRUE)]

# Get weights:

movie_w = learn %>% get_weights(top_movies, is_item = TRUE, convert = TRUE)

# Visualize with highcharter:

rownames(movie_w) = res$title

highcharter::hchart(princomp(movie_w, cor = TRUE)) %>% 
  highcharter::hc_legend(enabled = FALSE)


