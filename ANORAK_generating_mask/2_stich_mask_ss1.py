# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


import numpy as np
import pickle
import os
from glob import glob
import cv2
import re


def natural_key(string_):
    return [int(s) if s.isdigit() else s for s in re.split(r'(\d+)', string_)]

def printProgressBar(iteration, total, prefix='', suffix='', decimals=1, length=100, fill='='):  # chr(0x00A3)
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration / total)
    bar = fill * filledLength + '>' + '.' * (length - filledLength)
    print('\r%s |%s| %s%% %s' % (prefix, bar, percent, suffix), end="")
    # Print New Line on Complete
    if iteration == total:
        print()

####setup####
data_dir = 'input_folder_of_each_slide_which_has_been_divided_into_tiles'
mask_dir = 'ouput_folder_of_each_slide'
stich_dir = 'stiched_folder_of_each_slide'
slide_pattern = '*.ndpi'  #can be changed according to the slide format
annotated_files = sorted(glob(os.path.join(mask_dir, slide_pattern)))
####setup###

if os.path.exists(stich_dir) is False:
    os.makedirs(stich_dir)


for i in range(0, len(annotated_files)):

    wsi_name = os.path.split(annotated_files[i])[-1]

    if os.path.exists(os.path.join(stich_dir, wsi_name + "_Ss1.png")):
        print(wsi_name, 'exists')
        pass
    else:
        filename = os.path.split(annotated_files[i])[-1]

        param = pickle.load(open(os.path.join(data_dir, filename, 'param.p'), 'rb'))
        ss1 = cv2.imread(os.path.join(data_dir, filename, 'Ss1.jpg'))

        slide_dimension = np.array(param['slide_dimension']) / param['rescale']

        slide_w, slide_h = slide_dimension
        cws_w, cws_h = param['cws_read_size']

        divisor_w = np.ceil(slide_w / cws_w)
        divisor_h = np.ceil(slide_h / cws_h)

        w, h = int(slide_w * 0.0625), int(slide_h * 0.0625)
        print('\n%s, Target size: %i,%i, level_2 size: %i,%i' % (os.path.basename(filename), w, h, ss1.shape[1], ss1.shape[0]))
        img_all = np.zeros((max(h, ss1.shape[0])+1, max(ss1.shape[1],w)+1, 3))

        drivepath, imagename = os.path.split(annotated_files[i])
        annotated_dir_slide = os.path.join(mask_dir, imagename)
        annotated_path = annotated_dir_slide
        images = sorted(os.listdir(annotated_path), key=natural_key)
        printProgressBar(0, len(images), prefix='Progress:', suffix='Complete', length=50)
        i_count = 0
        for ii in images:
            i_count += 1

            cws_i = int(re.search(r'\d+', ii).group())
            h_i_ori = int(np.floor(cws_i / divisor_w)) * cws_h
            w_i = int((cws_i - h_i_ori / cws_h * divisor_w)) * int(cws_w * 0.0625)
            h_i = int(np.floor(cws_i / divisor_w)) * int(cws_h * 0.0625)

            img = cv2.imread(os.path.join(annotated_path, ii))
            if i_count == 1:
                cws_real_h = img.shape[0]
                cws_real_w = img.shape[1]
            cws_divisor_h = cws_h / img.shape[0]
            cws_divisor_w = cws_w / img.shape[1]
            w_r = max(int(img.shape[1] * 0.0625), 1)
            h_r = max(int(img.shape[0] * 0.0625), 1)
            img_r = cv2.resize(img, (w_r, h_r), interpolation=cv2.INTER_NEAREST)
            img_all[h_i: h_i + int(img_r.shape[0]), w_i: w_i + int(img_r.shape[1]), :] = img_r
            printProgressBar(cws_i, len(images), prefix='Progress:', suffix='Completed for %s' % i, length=50)

            if i_count % 20 == 0 or i_count >= len(images):
                cv2.imwrite(os.path.join(stich_dir, imagename + "_Ss1.png"), img_all)
