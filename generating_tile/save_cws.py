import cws_generator as cws
import os
import glob
from multiprocessing import Pool
from functools import partial

def cws_all_process(obj):
    obj.generate_cws()
    obj.slide_thumbnail()
    obj.param()
    obj.final_scan_ini()
    obj.clust_tile_sh()
    obj.da_tile_sh()


def single_file_run(file_name, output_dir, input_dir, tif_obj=40, cws_objective_value=20, in_mpp=None, out_mpp=None, out_mpp_target_objective=40, parallel=False):
    # print(file_name, flush=True)
    _, file_type = os.path.splitext(file_name)

    if file_type == '.svs' or file_type == '.ndpi' or file_type == '.mrxs':
        cws_obj = cws.CWSGENERATOR(output_dir=output_dir, file_name=file_name, input_dir=input_dir, cws_objective_value=cws_objective_value, in_mpp=in_mpp, out_mpp=out_mpp, out_mpp_target_objective=out_mpp_target_objective, parallel=parallel)
        cws_all_process(obj=cws_obj)

    if file_type == '.tif' or file_type == '.tiff' or file_type == '.png' or file_type == '.qptiff':
        cws_obj = cws.CWSGENERATOR(output_dir=output_dir, file_name=file_name, input_dir=input_dir, cws_objective_value=cws_objective_value, objective_power=tif_obj, in_mpp=in_mpp, out_mpp=out_mpp, out_mpp_target_objective=out_mpp_target_objective, parallel=parallel)
        cws_all_process(obj=cws_obj)


def run(opts_in, file_name_pattern='*.ndpi', num_cpu=12):
    output_dir = opts_in['output_dir']
    wsi_input = opts_in['wsi_input']
    tif_obj = opts_in['tif_obj']
    cws_objective_value = opts_in['cws_objective_value']
    in_mpp = opts_in['in_mpp']
    out_mpp = opts_in['out_mpp']
    out_mpp_target_objective=opts_in['out_mpp_target_objective']
    parallel = opts_in['parallel']

    if not os.path.isdir(output_dir):
        os.makedirs(output_dir, exist_ok=True)

    if os.path.isdir(wsi_input):
        files_all = sorted(glob.glob(os.path.join(wsi_input, file_name_pattern)))
        with Pool(num_cpu) as p:
            p.map(partial(single_file_run,
                          output_dir=output_dir,
                          input_dir=wsi_input,
                          tif_obj=tif_obj,
                          cws_objective_value=cws_objective_value,
                          in_mpp=in_mpp,
                          out_mpp=out_mpp,
                          out_mpp_target_objective=out_mpp_target_objective,
                          parallel=parallel),
                  files_all)

    if os.path.isfile(wsi_input):
        input_dir, file_name = os.path.split(wsi_input)
        single_file_run(file_name=file_name, output_dir=output_dir, input_dir=input_dir, tif_obj=tif_obj, cws_objective_value=cws_objective_value, in_mpp=in_mpp, out_mpp=out_mpp, out_mpp_target_objective=out_mpp_target_objective, parallel=parallel)
