import os
import cv2
import numpy as np
from glob import glob
from skimage.morphology import remove_small_objects

def remove_small(mask_digit, m, n, j, num):
    bw = np.zeros((m, n), dtype=bool)
    I = np.zeros((m, n), dtype=int)
    bw[mask_digit == j] = True
    mask_post = remove_small_objects(bw, num)
    I[mask_post] = j
    return I

#openCV: BGR
class_colors = [(0, 0, 0), (0, 255, 0), (255, 0, 255), (0, 0, 128), (0, 255, 255), (0, 0, 255), (255, 0, 0)]
def get_colored_segmentation_image(seg_arr, n_classes, colors=class_colors):
    output_height = seg_arr.shape[0]
    output_width = seg_arr.shape[1]
    seg_img = np.zeros((output_height, output_width, 3))
    for c in range(n_classes):
        seg_arr_c = seg_arr[:, :] == c
        seg_img[:, :, 0] += ((seg_arr_c)*(colors[c][0])).astype('uint8')
        seg_img[:, :, 1] += ((seg_arr_c)*(colors[c][1])).astype('uint8')
        seg_img[:, :, 2] += ((seg_arr_c)*(colors[c][2])).astype('uint8')
    return seg_img

def ss1_final(cws_folder, ss1_dir, ss1_final_dir, nfile=0, file_pattern='*.ndpi'):
    if not os.path.exists(ss1_final_dir):
        os.makedirs(ss1_final_dir)

    cws_files = sorted(glob(os.path.join(cws_folder, file_pattern)))
    file_name = os.path.split(cws_files[nfile])[-1]
    print(file_name)
    img = cv2.imread(os.path.join(ss1_dir, file_name+'_Ss1.png'))
    img = np.float32(cv2.cvtColor(img, cv2.COLOR_BGR2RGB))
    m, n, _ = img.shape
    mask_digit = np.zeros((m, n))

    mask_digit[(img[:, :, 0] == 0) & (img[:, :, 1] == 255) & (img[:, :, 2] == 0)] = 1  # cribriform
    mask_digit[(img[:, :, 0] == 255) & (img[:, :, 1] == 0) & (img[:, :, 2] == 255)] = 2  # micropapillary
    mask_digit[(img[:, :, 0] == 128) & (img[:, :, 1] == 0) & (img[:, :, 2] == 0)] = 3  # solid
    mask_digit[(img[:, :, 0] == 255) & (img[:, :, 1] == 255) & (img[:, :, 2] == 0)] = 4  # papillary
    mask_digit[(img[:, :, 0] == 255) & (img[:, :, 1] == 0) & (img[:, :, 2] == 0)] = 5  # acinar
    mask_digit[(img[:, :, 0] == 0) & (img[:, :, 1] == 0) & (img[:, :, 2] == 255)] = 6  # lepidic

    mask_post_digit = np.zeros((m, n))

    if np.any(mask_digit == 1):
        mask_post_digit = np.maximum(mask_post_digit, remove_small(mask_digit, m, n, 1, 300))

    if np.any(mask_digit == 2):
        mask_post_digit = np.maximum(mask_post_digit, remove_small(mask_digit, m, n, 2, 0))

    if np.any(mask_digit == 3):
        mask_post_digit = np.maximum(mask_post_digit, remove_small(mask_digit, m, n, 3, 300))

    if np.any(mask_digit == 4):
        mask_post_digit = np.maximum(mask_post_digit, remove_small(mask_digit, m, n, 4, 300))

    if np.any(mask_digit == 5):
        mask_post_digit = np.maximum(mask_post_digit, remove_small(mask_digit, m, n, 5, 0))

    if np.any(mask_digit == 6):
        mask_post_digit = np.maximum(mask_post_digit, remove_small(mask_digit, m, n, 6, 1000))

    mask_post = get_colored_segmentation_image(mask_post_digit, 7, colors=class_colors)
    cv2.imwrite(os.path.join(ss1_final_dir, file_name+'_Ss1.png'), mask_post)



