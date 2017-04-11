# This is my attempt to take some of our shark data and pass it through a
# neural network. Neural networks don't need us to supply features,
# so it's a good way to look at the data without analyzing it too closely.

# To run this code, you first need to install tensorflow.
# See https://github.com/rstudio/tensorflow for precise details.

# Just verifying everything
library(tensorflow)
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)
