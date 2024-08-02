from PIL import Image
import os

import imageio.v2 as image
directory="/home/musca/aabhat/phd/Projects/D6/test/10^8_withouttail_0,95/png/"

output_file="/home/musca/aabhat/phd/Projects/D6/test/10^8_withouttail_0,95/png/movie.mp4"
'''
files = sorted([file for file in os.listdir(directory) if file.endswith('.png')])

frames=[]

for file in files:
    file_path=os.path.join(directory,file)
    image=imageio.imread(file_path)
    frames.append(image)

imageio.mimsave(output_file,frames,fps=20)
'''
from moviepy.editor import ImageSequenceClip

def resize_images(image_folder, output_folder, target_size):
    # Create the output folder if it doesn't exist
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    # Resize images
    for filename in os.listdir(image_folder):
        if filename.endswith('.png'):
            img_path = os.path.join(image_folder, filename)
            img = Image.open(img_path)
            img_resized = img.resize(target_size, Image.LANCZOS)
            img_resized.save(os.path.join(output_folder, filename))

def create_video_from_images(image_folder, output_video_file, fps=20):
    # List all files in the directory
    files = sorted([os.path.join(image_folder, f) for f in os.listdir(image_folder) if f.endswith('.png')])

    # Load images
    clip = ImageSequenceClip(files, fps=fps)

    # Write the video file
    clip.write_videofile(output_video_file)#, codec='libx264')

if __name__ == "__main__":
    #image_folder = "path/to/your/png/folder"  # Replace with the path to your folder with PNGs
    #output_video_file = "output_video.mp4"    # Replace with your desired output video file name
    #target_size = (1920,1080)  # Replace with your desired target size (width, height)
    
    
    # Resize images
    #resize_images(directory, directory, target_size)

    
    # Create video from resized images
    create_video_from_images(directory, output_file)


