## AIgrading
This is an implementation for "Artificial Intelligence-based grading of lung adenocarcinoma growth patterns".  
This repository could guide you to generate growth pattern masks with a well-trained deep learning model for semantic segmentation, from which the proportion of each growth pattern can be obtained, thereby replicating IASLC grading for lung adenocarcinoma.

### Training
dataset: $768 \times 768$ at x20 (~0.45mm/pixel) or $384 \times 384$ at 10x (~0.9mm/pixel)  
training time:  
`module load anaconda/3  
source /opt/software/applications/anaconda/3/etc/profile.d/conda.sh  
conda activate /home/xpan/.conda/envs/tfGPU2p2  
cd /your_own_dir/ANORAK_training  
python train_main.py
conda deactivate`


### Inference
### Input: H&E image tile, ideally larger than $768 \times 768$
### Output: Growth pattern mask, black-background, blue-lepidic, yellow-papillary, red-acinar, green-cribriform, magenta-micropapillary, dark red-solid
