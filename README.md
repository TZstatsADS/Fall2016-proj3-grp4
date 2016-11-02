# Project: Labradoodle or Fried Chicken? 
![image](https://s-media-cache-ak0.pinimg.com/236x/6b/01/3c/6b013cd759c69d17ffd1b67b3c1fbbbf.jpg)
### [Full Project Description](doc/project3_desc.html)

Term: Fall 2016

+ Team #4
+ Team members
	+ Weichuan Wu
	+ Kyongmook Lim
	+ Yunyi Zhang
	+ William Raikes
	+ Tian Sheng
+ Project summary: In this project, we created a classification engine for images of poodles versus images of fried chickens. We tried sift features, pixels of resized images and some rgb features we extra from the images, and we also tried different classifers such as GBM, SVM, randomForest or LDA, it turns out that changing the features from sift to piexl or rgb features make a huge improvement in the prediction accuracy. And among all the classifiers, the SVM with kernel radial perferce the best with a cross validation accuracy about 88% using the rgb features. 
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) 
+ Weichuan Wu: 

	+ Build the baseline model and construct other models' structures.
	+ Use PCA to process the original features data so that speed up the training process.
	+ Write the feature.R, train.R,test.R ,evaluation.R and main.R so that they can meet the requirements.
+ Yunyi Zhang: 

	+ Use different methods to train classification models on SIFT features and test the models' performance.  
	+ Extract the RGB color feature from the images and test their applicability for classification 
	+ Combine RGB color feature and SIFT feature together to look for improvement in classification 
+ Kyongmook Lim: 

	+ made presentation powerpoint, and researched about gbm, adaboost, bernoulli .  
+ TianSheng: 

	+ Tested the neuron network classifier with an accuracy of 60%.
	+ Explored different kinds of filtering methods in OpenCV: bilateral, median, Gaussian, threshold, etc.
	+ Extracted color features like color histogram and mean color with OpenCV.
       
Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
