import os
import openslide
import numpy as np
import math
from PIL import Image
import pickle
import scipy.io as sio
import tifffile
import warnings
import multiprocessing

class CWSGENERATOR:

    def __init__(self,
                 input_dir=os.getcwd(),
                 file_name='Test_file.svs',
                 output_dir=os.path.join(os.getcwd(), 'cws'),
                 cws_objective_value=20,
                 objective_power=0,
                 cws_read_size_w=2000,
                 cws_read_size_h=2000,
                 in_mpp=None,
                 out_mpp=None,
                 out_mpp_target_objective=40,
                 parallel=False):
        self.input_dir = input_dir
        self.file_name = os.path.basename(file_name)
        if not os.path.isdir(output_dir):
            os.makedirs(output_dir)
        self.output_dir = os.path.join(output_dir, self.file_name)
        if not os.path.isdir(self.output_dir):
            os.makedirs(self.output_dir)
        self.cws_read_size = np.array([cws_read_size_w, cws_read_size_h])
        self.openslide_obj = openslide.OpenSlide(filename=os.path.join(self.input_dir, self.file_name))
        self.parallel = parallel

        if objective_power == 0:
            self.objective_power = float(self.openslide_obj.properties[openslide.PROPERTY_NAME_OBJECTIVE_POWER])
        else:
            self.objective_power = objective_power
        
        if out_mpp is None:
            self.cws_objective_value = cws_objective_value
        elif in_mpp is None:
            _, file_type = os.path.splitext(self.file_name)
            
            try:
                if file_type == '.tif' or file_type == '.tiff' or file_type == '.qptiff':
                    with tifffile.TiffFile(os.path.join(self.input_dir, self.file_name)) as tif:
                        in_mpp = tif.pages[0].tags['XResolution'].value
                        in_mpp = 10000*in_mpp[1]/in_mpp[0]
                else:
                    in_mpp = float(self.openslide_obj.properties[openslide.PROPERTY_NAME_MPP_X])
            except:
                warnings.warn("Warning: Automatic detection of file's scanning resolution failed. Resolution correction will not be applied to this image.")
        
            if in_mpp is None:
                self.cws_objective_value = cws_objective_value
            else:
                self.cws_objective_value = cws_objective_value*(self.objective_power/out_mpp_target_objective)*(in_mpp/out_mpp)
        else:
            self.cws_objective_value = cws_objective_value*(self.objective_power/out_mpp_target_objective)*(in_mpp/out_mpp)

    def generate_cws(self):
        def initialise_par(wsi_path):
            global openslide_obj
            
            openslide_obj = openslide.OpenSlide(filename=wsi_path)

        openslide_obj = self.openslide_obj
            
        if self.parallel:
            self.openslide_obj = None #Deactivate the main process object so it doesn't get sent to subprocesses
        
        if self.objective_power == 0:
            self.objective_power = float(openslide_obj.properties[openslide.PROPERTY_NAME_OBJECTIVE_POWER])

        self.rescale = self.objective_power / self.cws_objective_value
        slide_dimension = openslide_obj.level_dimensions[0]
        self.slide_h = slide_dimension[1]
        self.slide_w = slide_dimension[0]
        self.rescale_slide_h = int(round(self.slide_h / self.rescale))
        self.rescale_slide_w = int(round(self.slide_w / self.rescale))
        openslide_read_size = np.multiply(self.cws_read_size, self.rescale)
        self.cws_h = openslide_read_size[0]
        self.cws_w = openslide_read_size[1]

        self.yTiles = int(math.ceil((self.slide_h - self.cws_h) / self.cws_h + 1))
        self.xTiles = int(math.ceil((self.slide_w - self.cws_w) / self.cws_w + 1))
        self.nTiles = self.yTiles*self.xTiles
        
        if self.parallel:
            with multiprocessing.Pool(processes=max(1, multiprocessing.cpu_count()-1), initializer=initialise_par, initargs=(os.path.join(self.input_dir, self.file_name),)) as pool:
                pool.map(self.write_tile, range(self.nTiles))
                
            self.openslide_obj = openslide_obj #Reactivate the main process object
        else:
            self.text_output = open(os.path.join(self.output_dir, 'Output.txt'), 'w')
            
            for i in range(self.nTiles):
                self.write_tile(i)
                
            self.text_output.close()
            delattr(self, 'text_output')
            
    def write_tile(self, i):
        if self.parallel:
            self.openslide_obj = openslide_obj
    
        h = i//self.xTiles
        w = i%self.xTiles
                
        start_h = int(round(h * self.cws_h))
        end_h = int(round((h * self.cws_h) + self.cws_h))
        start_w = int(round(w * self.cws_w))
        end_w = int(round((w * self.cws_w) + self.cws_w))

        out_h = self.cws_read_size[1]
        out_w = self.cws_read_size[0]

        if end_h > self.slide_h:
            end_h = self.slide_h
            out_h = self.rescale_slide_h - (h * self.cws_read_size[1])

        if end_w > self.slide_w:
            end_w = self.slide_w
            out_w = self.rescale_slide_w - (w * self.cws_read_size[0])

        im = self.openslide_obj.read_region([start_w, start_h], 0, [end_w - start_w, end_h - start_h])
        format_str = 'Da%d:  start_w:%d, end_w:%d, start_h:%d, end_h:%d, width:%d, height:%d'
        if not self.parallel:
            self.text_output.write((format_str + '\n') % (
                i, start_w, end_w, start_h, end_h, end_w - start_w, end_h - start_h))
        # print(format_str % (
        #     i, start_w, end_w, start_h, end_h, end_w - start_w, end_h - start_h), flush=True)
        temp = np.array(im)
        temp = temp[:, :, 0:3]
        im = Image.fromarray(temp)
        if self.rescale != 1:
            im = im.resize(size=[out_w, out_h],
                           resample=Image.BICUBIC)
        im.save(os.path.join(self.output_dir, 'Da' + str(i) + '.jpg'), format='JPEG')

    def slide_thumbnail(self):
        openslide_obj = self.openslide_obj
        cws_objective_value = self.cws_objective_value
        output_dir = self.output_dir
        file_name = self.file_name

        if self.objective_power == 0:
            self.objective_power = float(openslide_obj.properties[openslide.PROPERTY_NAME_OBJECTIVE_POWER])

        _, file_type = os.path.splitext(file_name)
        rescale = self.objective_power / cws_objective_value
        slide_dimension = openslide_obj.level_dimensions[0]

        if file_type == '.tif' or file_type == '.tiff' or file_type == '.qptiff':
            slide_dimension_rescale = np.round(np.array(slide_dimension) / rescale)
            slide_dimension_ss1 = (slide_dimension_rescale / 16).astype(np.int32)
            cws_read_size = np.array(self.cws_read_size)
            cws_read_size_ss1 = np.round(cws_read_size/16).astype(np.int32)
            slide_dimension_thumb = np.round(np.multiply(slide_dimension_rescale, np.divide(cws_read_size_ss1, cws_read_size))).astype(np.int32)
            thumb = self.create_thumb_from_tiles(slide_dimension_thumb, cws_read_size_ss1)
            thumbSS1 = thumb.resize(size=slide_dimension_ss1, resample=Image.BICUBIC)
            thumbSS1.save(os.path.join(output_dir, 'Ss1.jpg'), format='JPEG')
            scale_h = round(slide_dimension[0]) / 1024
            thumbnail_height = int(round(slide_dimension[1] / scale_h))
            thumb = thumb.resize(size=[1024, thumbnail_height], resample=Image.BICUBIC)
            thumb.save(os.path.join(output_dir, 'SlideThumb.jpg'), format='JPEG')
        else:
            slide_dimension_rescale = np.round(np.array(slide_dimension) / rescale)
            slide_dimension_ss1 = (slide_dimension_rescale / 16).astype(np.int32)
            scale_h = round(slide_dimension[0]) / 1024
            thumbnail_height = int(round(slide_dimension[1] / scale_h))
            thumb = openslide_obj.get_thumbnail([1024, thumbnail_height])
            thumb.save(os.path.join(output_dir, 'SlideThumb.jpg'), format='JPEG')
            thumb = openslide_obj.get_thumbnail(slide_dimension_ss1)
            thumb.save(os.path.join(output_dir, 'Ss1.jpg'), format='JPEG')

    def create_thumb_from_tiles(self, thumb_size, out_tile_size):
        tile_dir = self.output_dir

        thumb = np.zeros((thumb_size[1], thumb_size[0], 3), dtype=np.uint8)

        i = 0
        for h in range(int(math.ceil((thumb_size[1] - out_tile_size[1]) / out_tile_size[1] + 1))):
            for w in range(int(math.ceil((thumb_size[0] - out_tile_size[0]) / out_tile_size[0] + 1))):
                start_h = int(round(h * out_tile_size[1]))
                end_h = int(round((h * out_tile_size[1]) + out_tile_size[1]))
                start_w = int(round(w * out_tile_size[0]))
                end_w = int(round((w * out_tile_size[0]) + out_tile_size[0]))

                if end_h > thumb_size[1]:
                    end_h = thumb_size[1]

                if end_w > thumb_size[0]:
                    end_w = thumb_size[0]

                im = Image.open(os.path.join(tile_dir, 'Da' + str(i) + '.jpg'))
                im = im.resize(size=[end_w-start_w, end_h-start_h], resample=Image.BICUBIC)
                thumb[start_h:end_h, start_w:end_w, :] = np.array(im)
                    
                i += 1

        return Image.fromarray(thumb)


    def param(self):
        exp_dir = self.output_dir
        if self.objective_power == 0:
            self.objective_power = float(self.openslide_obj.properties[openslide.PROPERTY_NAME_OBJECTIVE_POWER])
        objective_power = self.objective_power
        slide_dimension = self.openslide_obj.level_dimensions[0]
        cws_objective_value = self.cws_objective_value
        rescale = objective_power / cws_objective_value
        filename = self.file_name
        cws_read_size = self.cws_read_size

        param = {'exp_dir': exp_dir,
                 'objective_power': objective_power,
                 'slide_dimension': slide_dimension,
                 'rescale': rescale,
                 'cws_objective_value': cws_objective_value,
                 'filename': filename,
                 'cws_read_size': cws_read_size}
        pickle.dump(param, open(os.path.join(exp_dir, 'param.p'), 'wb'))
        sio.savemat(os.path.join(exp_dir, 'param.mat'), param)

    def final_scan_ini(self):
        cws_objective_value = self.cws_objective_value
        if self.objective_power == 0:
            self.objective_power = float(self.openslide_obj.properties[openslide.PROPERTY_NAME_OBJECTIVE_POWER])
        rescale = self.objective_power / cws_objective_value
        output_dir = self.output_dir
        openslide_obj = self.openslide_obj
        cws_read_size = self.cws_read_size
        slide_dimension = openslide_obj.level_dimensions[0]
        slide_dimension = np.round(np.array(slide_dimension)/rescale)
        slide_dimension = slide_dimension.astype(np.int32)
        slide_h = slide_dimension[1]
        slide_w = slide_dimension[0]
        cws_h = cws_read_size[0]
        cws_w = cws_read_size[1]
        text_output = open(os.path.join(output_dir, 'FinalScan.ini'), 'w')
        text_output.write('[Header]\n'
                          'iVersion=1.1DigitalSLideStudioLinux\n'
                          'tOperatorID=d1a91842-dec4-4a0a-acd2-1e4a47cae935\n'
                          'tTimeOfScan=%DATE%\n'
                          'tWebSlideTitle=\n'
                          'lXStageRef=50896\n'
                          'lYStageRef=32666\n'
                          )
        text_output.write('iImageWidth=%d\n' % (self.cws_read_size[0]))
        text_output.write('iImageHeight=%d\n' % (self.cws_read_size[1]))
        text_output.write('lXStepSize=8000\n'
                          'lYStepSize=8000\n'
                          'lXOffset=0\n'
                          'lYOffset=0\n'
                          'dMagnification=%MAGNIFICATION%\n'
                          'tImageType=.jpg\n'
                          'iFinalImageQuality=80\n'
                          'iAnalysisImageCount=%AMOUNTOFTILES%\n'
                          'iCalibrationImageCount=0\n'
                          'AIL=12.0.14 \n'
                          'bHasThumb=1\n'
                          'bHasMacro=1\n'
                          'bHasLabel=1\n'
                          'iLayers=1\n'
                          'iLevels=%AMOUNTOFLEVELS%\n'
                          'tMPP=0.496900\n'
                          )
        text_output.write('tDescription=Aperio Image Library v12.0.14   %dx%d (%dx%d) JPEG '
                          'Q=80;Aperio Image Library vFS90 01  58880x48433 [0,100 57448x48333] (256x256) JPEG/RGB '
                          'Q=70|AppMag = 20|StripeWidth = 1840|ScanScope ID = SS5306|Filename = 90284|'
                          'Date = 05/03/13|Time = 16:03:31|Time Zone = GMT+01:00|'
                          'User = d1a91842-dec4-4a0a-acd2-1e4a47cae935|Parmset = COVERSLIP|MPP = 0.4969|'
                          'Left = 25.313839|Top = 24.087423|LineCameraSkew = 0.000794|LineAreaXOffset = 0.008057|'
                          'LineAreaYOffset = -0.005012|Focus Offset = -0.000500|DSR ID = panacea|ImageID = 90284|'
                          'Exposure Time = 109|Exposure Scale = 0.000001|DisplayColor = 0|OriginalWidth = 58880|'
                          'OriginalHeight = 48433|ICC Profile = ScanScope v1\n'
                          % (slide_dimension[0], slide_dimension[1], self.cws_read_size[0], self.cws_read_size[1]))
        text_output.write('tTotalFileSize=624223872\n'
                          '[Level0]\n'
                          'iZoom=1\n'
                          )
        text_output.write('iWidth=%d\n'
                          'iHeight=%d\n'
                          % (slide_dimension[0], slide_dimension[1]))
        text_output.write('iQuality=80\n'
                          '[Level1]\n'
                          'iZoom=4\n'
                          'iWidth=14362\n'
                          'iHeight=12083\n'
                          'iQuality=90\n'
                          '[Level2]\n'
                          'iZoom=16\n'
                          'iWidth=3590\n'
                          'iHeight=3020\n'
                          'iQuality=95\n')
        i = 0
        for h in range(int(math.ceil((slide_h - cws_h) / cws_h + 1))):
            for w in range(int(math.ceil((slide_w - cws_w) / cws_w + 1))):
                x_text = ((slide_dimension[0] / 2)-(w*cws_w + cws_read_size[0]/2))*4
                y_text = ((slide_dimension[1] / 2) - (h*cws_h + cws_read_size[1] / 2)) * 4
                text_output.write('[Da%d]\n'
                                  'x=%d\n'
                                  'y=%d\n'
                                  'z=0\n' % (i, x_text, y_text))
                i += 1
        
        text_output.close()

    def clust_tile_sh(self):
        output_dir = self.output_dir
        filename = self.file_name
        text_output = open(os.path.join(output_dir, 'clustTile.sh'), 'w')
        text_output.write('\n'
                          '#BSUB -J "%s"\n'
                          '#BSUB -o output/%s.%%J\n'
                          '#BSUB -e errors/%s.%%J\n'
                          '#BSUB -n 1\n'
                          '#BSUB -P DMPYXYAAO\n'
                          '#BSUB -W 15:00\n'
                          'startRowByRowTile.sh\n'
                          'startFinalTile.sh\n'
                          % (filename, filename, filename))
        text_output.close()

    def da_tile_sh(self):
        output_dir = self.output_dir
        openslide_obj = self.openslide_obj
        cws_objective_value = self.cws_objective_value
        cws_read_size = self.cws_read_size
        if self.objective_power == 0:
            self.objective_power = float(openslide_obj.properties[openslide.PROPERTY_NAME_OBJECTIVE_POWER])
        rescale = self.objective_power / cws_objective_value
        openslide_read_size = np.multiply(cws_read_size, rescale)

        slide_dimension = openslide_obj.level_dimensions[0]
        slide_h = slide_dimension[1]
        slide_w = slide_dimension[0]
        cws_h = openslide_read_size[0]
        cws_w = openslide_read_size[1]
        text_output = open(os.path.join(output_dir, '_daTile.sh'), 'w')
        text_output.write('montage -limit area 12192 -limit memory 8192 \\ \n')
        i = 0
        for h in range(int(math.ceil((slide_h - cws_h) / cws_h + 1))):
            for w in range(int(math.ceil((slide_w - cws_w) / cws_w + 1))):
                text_output.write('Da%d.jpg \\ \n' % i)
                i += 1

        text_output.write('-mode Concatenate -tile  %dx%d Da_tiled.jpg \n'
                          % (int(math.ceil((slide_w - cws_w) / cws_w + 1)),
                             int(math.ceil((slide_h - cws_h) / cws_h + 1)))
                          )
        text_output.close()
