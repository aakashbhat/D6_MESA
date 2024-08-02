import os
import imageio.v2 as imageio

directory="/home/musca/aabhat/phd/Projects/D6/test/10^8_withouttail_0,95/png/"
output_file="/home/musca/aabhat/phd/Projects/D6/test/10^8_withouttail_0,95/png/movie.mp4"

files = sorted([file for file in os.listdir(directory) if file.endswith('.png')])

frames=[]

for file in files:
    file_path=os.path.join(directory,file)
    image=imageio.imread(file_path)
    frames.append(image)

imageio.mimsave(output_file,frames,fps=20)



