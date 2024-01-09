import os
import argparse
import tensorflow as tf
import numpy as np
import cv2
from PIL import Image
import math
from glob import glob

from predict_gp import generate_gp
from ss1_stich import ss1_stich
from ss1_final import ss1_final

# get arguments
parser = argparse.ArgumentParser()
parser.add_argument('-d', '--data_dir', dest='data_dir', help='path to cws data')
parser.add_argument('-o', '--save_dir', dest='save_dir', help='path to save all output files', default=None)
parser.add_argument('-s', '--save_dir_ss1', dest='save_dir_ss1', help='path to save all ss1 files', default=None)
parser.add_argument('-sf', '--save_dir_ss1_final', dest='save_dir_ss1_final', help='path to save all final files', default=None)
parser.add_argument('-p', '--pattern', dest='file_name_pattern', help='pattern in the files name', default='*.ndpi')
parser.add_argument('-c', '--color', dest='color_norm', help='color normalization', action='store_false')
parser.add_argument('-n', '--nfile', dest='nth_file', help='the n-th file', default=0, type=int)

if __name__ == '__main__':
    args = parser.parse_args()
    ######step0: generate cws tiles from single-cell pipeline
    ######step1: generate growth pattern for tiles
    generate_gp(datapath=args.data_dir, save_dir=args.save_dir, file_pattern=args.file_name_pattern, color_norm=args.color_norm, nfile=args.nth_file,
                patch_size=768, patch_stride=192, nClass=7)

    #######step2: stich to ss1 level
    ss1_stich(cws_folder=args.data_dir, annotated_dir=args.save_dir, output_dir=args.save_dir_ss1, nfile=args.nth_file, file_pattern=args.file_name_pattern)

    #######step3: post-processing
    ss1_final(cws_folder=args.data_dir, ss1_dir=args.save_dir_ss1, ss1_final_dir=args.save_dir_ss1_final, nfile=args.nth_file, file_pattern=args.file_name_pattern)






