#code for preparing the hindcast figures in pdf format for creating an animation

library(tidyverse)
library(terra)


#from terminal
ffmpeg -framerate 5 -pattern_type glob -i "*.png" hindcast_pa.mp4

#create a text file with the plot titles separated by line breaks

files <- list.files("/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/cu_all_animation_pa/pngs_pa",
                    pattern = ".png")

time_periods <- str_sub(files, 13, -9)[-72] #remove the "present"

#add comma 
time_periods <- formatC(as.numeric(time_periods), format = "d", big.mark = ",")

pdf_titles <- c(paste(time_periods, "years ago"), "Present")

#save a text file

write.table(pdf_titles, "/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/pdf_titles.csv", 
            row.names = F, col.names = F, dec = ".", sep = ";")

write.table(pdf_titles, "/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/pdf_titles.txt", 
            row.names = F, col.names = F, dec = ".", sep = ";", eol = "\r\n")


#code for making the animation while adding text to each pdf file
ffmpeg -framerate 5 -pattern_type glob -i "*.png" hindcast_pa.mp4

ffmpeg -i hindcast_cu_all_pa.mp4 -vf "drawtext=fontfile=Arial:textfile=/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/pdf_titles.txt:x=(w-tw)/2:y=h/10:fontcolor=black:fontsize=24" -c:a copy hindcaset_pa_titles.mp4

ffmpeg -i hindcast_cu_all_pa.mp4 -vf "drawtext=fontfile=Arial:textfile=/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/pdf_titles.csv:reload=1:x=(w-tw)/2:y=h/10:fontcolor=black:fontsize=24" -c:a copy hindcaset_pa_titles.mp4



### try again

ffmpeg -framerate 5 -pattern_type glob -i "*.png" -vf "drawtext=Arial:text='@txt':x=10:y=10:fontcolor=black:fontsize=40" -c:v libx264 hindcast_pa.mp4

ffmpeg -framerate 5 -pattern_type glob -i "*.png" -vf "drawtext=fontfile=Arial:text='@txt':x=10:y=10:fontcolor=black:fontsize=24" -txt frame_texts.txt -c:v libx264 hindcast_test.mp4


ffmpeg -framerate 5 -pattern_type glob -i "*.png" -vf "drawtext=fontfile=Arial:text='@txt':x=20:y=20:fontcolor=white:fontsize=24" -c:v libx264 hindcast_pa.mp4

