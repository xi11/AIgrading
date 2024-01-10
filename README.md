## AI grading
This repository is for the paper  "The artificial intelligence-based model ANORAK improves histopathological grading of lung adenocarcinoma" published in Nature Cancer. It could guide you to generate growth pattern masks with a well-trained deep learning model for semantic segmentation, from which the proportion of each growth pattern can be obtained, thereby replicating IASLC grading for lung adenocarcinoma.

### Generating tiles for a whole slie image
Dependencies for generating_tiles are in AIgraind/generating_tiles/requirements, following the step in AbdulJabbar, K. et al. Geospatial immune variability illuminates differential evolution of lung adenocarcinoma. Nature Medicine (2020). doi: 10.1038/s41591-020-0900-x
After all dependencies are well installed, if the format of whole slide images are in .svs format, then
```
python ./generating_tiles/main_tiles.py -d /path/to/raw/slides -o /path/to/result -p '*.svs'
```
The ouptut structure will be
```
result_cws_tiling/
        ├── TCGA-xxxx-xxxx.svs
		├── Da0.jpg
		├── Da1.jpg
		└── ...
	├── TCGA-xxxx-xxxx.svs
		├── Da0.jpg
		├── Da1.jpg
		└── ...
```
### Training
Dependencies for training_patch are in AIgrading/requirements
dataset: $768 \times 768$ at x20 (~0.45um/pixel) or $384 \times 384$ at x10 (~0.9um/pixel)  
training time: ~5hrs under a single GPU (NVIDIA Tesla P100 PCIe 16 GB)
Step0: creat the conda environment following AIgrading/requirements.txt
Step1: divide image in trainset (download from 10.5281/zenodo.10016027) into patches with a size of $768 \times 768$ 
```
python ./training_patch/img_patch_768.py --image_path /path/to/training/image --label_path /path/to/training/mask --save_path /path/to/patches
```
The training patches are structured as
```
Training_patches /
├── image
│   ├── train001_xxx_0.png
│   ├── train001_xxx_1.png
│   └── ...
└── maskPng
    ├── train001_xxx_0.png
    ├── train001_xxx_1.png
    └── ...
```
Step2: train the model
```
python ./training_patch/train_main.py --input_dir /path/to/patches/image --target_dir /path/to/patches/maskPng --img_size 384 --num_class 7 --batch_size 8 --num_epoch 60
```

### Inference
Dependencies for inference_slide are in AIgraind/requirements, same with the training

Input: H&E image tiles

Output: Growth pattern mask, 
```
#000000 black-background, 
#0000ff blue-lepidic, 
#ffff00 yellow-papillary, 
#ff0000 red-acinar, 
#00ffff cyan-cribriform, 
#ff00ff magenta-micropapillary, 
#880000 dark red-solid
```
```
python ./inference_slide/main_gp.py -d /path/to/cws_tiling -o /path/to/cws_mask -s /path/to/ss1_mask -sf /path/to/ss1_mask_final -p '.svs' -n 0
```

