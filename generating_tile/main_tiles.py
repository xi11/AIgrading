import os
import argparse
import save_cws
from glob import glob

def run_generate_cws(wsi_input,
                     output_dir,
                     tif_obj=40,
                     cws_objective_value=20,
                     in_mpp=None,
                     out_mpp=None,
                     out_mpp_target_objective = 40,
                     file_name_pattern='*.ndpi',
                     parallel=False):
    opts = {
        'output_dir': output_dir,
        'wsi_input': wsi_input,
        'tif_obj': tif_obj,
        'cws_objective_value': cws_objective_value,
        'in_mpp': in_mpp,
        'out_mpp': out_mpp,
        'out_mpp_target_objective': out_mpp_target_objective,
        'parallel': parallel
            }
    save_cws.run(opts_in=opts, file_name_pattern=file_name_pattern, num_cpu=4)

# get arguments
parser = argparse.ArgumentParser()
parser.add_argument('-d', '--data_dir', dest='data_dir', help='path to raw image data')
parser.add_argument('-o', '--results_dir', dest='results_dir', help='path to save all output files', default=None)
parser.add_argument('-mpp', '--output_mpp', dest='output_mpp', help='output magnification', default=0.22, type=float)
parser.add_argument('-p', '--pattern', dest='file_name_pattern', help='pattern in the files name', default='*.ndpi')

if __name__=="__main__":
    args = parser.parse_args()
    wsi = sorted(glob(os.path.join(args.data_dir, args.file_name_pattern)))
    for wsi_i in wsi:
        run_generate_cws(wsi_input=wsi_i,
                         output_dir=os.path.join(args.results_dir, 'cws_tiling'),
                         tif_obj=40,
                         cws_objective_value=20,
                         in_mpp=None,
                         out_mpp=args.output_mpp,
                         file_name_pattern=args.file_name_pattern,
                         parallel=False)
