import os
import pathlib
import numpy as np
from scipy.io import loadmat
import math
import cv2
import argparse

def read_img(data_file, label_file):    # to be checked with if file or path
    data = cv2.imread(data_file)
    labels = loadmat(label_file)['mask']   # label is in .mat format
    return data, labels

def get_label_file(data_file,label_path): #get mask with corresponding name
    data_file_name = data_file.split('/')[-1]  #split path and get the last one, which is the file name; '/' for running under linux, '\\' for running under win
    label_file_name = data_file_name[:-4] + '.mat' #remove .jpg/.png, combined with '.mat'
    label_file = os.path.join(label_path, label_file_name)
    return label_file, data_file_name[:-4]

def write_to_patch(data_files, label_path, save_path):
    for file_n in range(0, len(data_files)):
        curr_data_file = str(data_files[file_n])
        curr_label_file, file_base_name = get_label_file(curr_data_file,label_path)
        data, labels = read_img(curr_data_file, curr_label_file)
        num_patch = extract_patches_img_label(data, labels, save_path, file_base_name, img_patch_h=768, img_patch_w=768, stride_h=768, stride_w=768, label_patch_h=768, label_patch_w=768)


def extract_patches_img_label(image, label, save_path, file_base_name, img_patch_h=384, img_patch_w=384, stride_h=384, stride_w=384, label_patch_h=384, label_patch_w=384):
    if max(image.shape[0],image.shape[1]) < 0:
        patch_img = image
        cv2.imwrite(os.path.join(save_path, 'image', file_base_name + '.png'), patch_img)
        mask = label
        cv2.imwrite(os.path.join(save_path, 'maskPng', file_base_name + '.png'), mask)

    else:
        if image.shape[0] < img_patch_h:
            pad_h = img_patch_h - image.shape[0]
        else:
            pad_h = 0

        if image.shape[1] < img_patch_w:
            pad_w = img_patch_w - image.shape[1]
        else:
            pad_w = 0

        image = np.lib.pad(image, ((0, pad_h), (0, pad_w), (0, 0)),'constant', constant_values=0)
        label = np.lib.pad(label, ((0, pad_h), (0, pad_w)), 'constant', constant_values=0)

        img_h = np.size(image, 0)
        img_w = np.size(image, 1)
        num_patches_img_h = math.ceil((img_h - img_patch_h) / stride_h + 1)
        num_patches_img_w = math.ceil(((img_w - img_patch_w) / stride_w + 1))
        num_patches_img = num_patches_img_h*num_patches_img_w

        iter_tot = 0
        img_patches = np.zeros((num_patches_img, img_patch_h, img_patch_w, image.shape[2]), dtype=image.dtype)
        label_patches = np.zeros((num_patches_img, label_patch_h, label_patch_w), dtype=image.dtype)
        for h in range(int(math.ceil((img_h - img_patch_h) / stride_h + 1))):
            for w in range(int(math.ceil((img_w - img_patch_w) / stride_w + 1))):
                start_h = h * stride_h
                end_h = (h * stride_h) + img_patch_h
                start_w = w * stride_w
                end_w = (w * stride_w) + img_patch_w
                if end_h > img_h:
                    start_h = img_h - img_patch_h
                    end_h = img_h

                if end_w > img_w:
                    start_w = img_w - img_patch_w
                    end_w = img_w

                patch_img = image[start_h:end_h, start_w:end_w, :]
                cv2.imwrite(os.path.join(save_path, 'image', file_base_name+'_'+str(iter_tot) + '.png'), patch_img)
                mask = label[start_h:end_h, start_w:end_w]
                cv2.imwrite(os.path.join(save_path, 'maskPng', file_base_name + '_' + str(iter_tot) + '.png'), mask)
                iter_tot += 1
        return iter_tot

def run(opts_in):
    save_path = opts_in['save_path']
    image_path = opts_in['image_path']
    label_path = opts_in['label_path']

    if not os.path.exists(save_path):
        os.makedirs(save_path)
        os.makedirs(os.path.join(save_path, 'image'))
        os.makedirs(os.path.join(save_path, 'maskPng'))
    train_data = sorted(list(image_path.glob('train*')))
    print('data number: ', len(train_data))
    write_to_patch(data_files = train_data, label_path = label_path, save_path=save_path)

parser = argparse.ArgumentParser(description='to generating patches')
parser.add_argument('--image_path', required=True, help='path to the image data')
parser.add_argument('--label_path', required=True, help='path to the label data')
parser.add_argument('--save_path', required=True, help='path to save the patches')

if __name__ == '__main__':
    args = parser.parse_args()
    opts = {
        'save_path': pathlib.Path(args.save_path),
        'image_path': pathlib.Path(args.image_path),
        'label_path': pathlib.Path(args.label_path),
    }
    run(opts_in=opts)
