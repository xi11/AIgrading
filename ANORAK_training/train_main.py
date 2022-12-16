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
import tensorflow as tf
import random as rn
import os
from pandas import DataFrame
from tensorflow import keras
from tensorflow.keras.preprocessing.image import load_img
from tensorflow.keras.models import *
from tensorflow.keras.layers import *
from tensorflow.keras import layers
from tensorflow.keras.optimizers import SGD, RMSprop, Adadelta, Adam
from tensorflow.keras.callbacks import ModelCheckpoint, LearningRateScheduler, EarlyStopping
import platform
from tensorflow.keras.callbacks import ModelCheckpoint
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from crossAttention import selfCrossPooling

np.random.seed(2021)
tf.random.set_seed(2021)
rn.seed(2021)



def scheduler(epoch, lr):
    if epoch <10:
        print("lr changed to {}".format(lr))
        return lr

    elif epoch >= 10 and epoch <50:
        print("lr changed to {}".format(0.0001))
        return 0.0001

    else:
        print("lr changed to {}".format(0.00001))
        return 0.00001

def to_categorical_mask(multi_label, nClasses):
    categorical_mask = np.zeros((multi_label.shape[0], multi_label.shape[1], nClasses))
    for c in range(nClasses):
        categorical_mask[:, :, c] = (multi_label == c).astype(int)
    categorical_mask = np.reshape(categorical_mask, (multi_label.shape[0] * multi_label.shape[1], nClasses))
    return categorical_mask

def random_adjust_saturation(image, min_delta=0.8, max_delta=2.0, max_delta_hue=0.1,seed=None):
    delta = tf.random.uniform([], -max_delta_hue, max_delta_hue, seed=seed)
    image = tf.image.adjust_hue(image / 255.0, delta)
    image = tf.clip_by_value(image, clip_value_min=0.0, clip_value_max=1.0)
    saturation_factor = tf.random.uniform([], min_delta, max_delta, seed=seed)
    image = tf.image.adjust_saturation(image, saturation_factor)
    image = tf.clip_by_value(image, clip_value_min=0.0, clip_value_max=1.0)
    return image

####setup####
train_img_dir = 'train_image_768'
train_mask_dir = 'train_mask_768' #0:background, 1:cribriform, 2:micropapillary, 3:solid, 4:papillary, 5:acinar, 6:lepidic
img_size = (384, 384)
nClasses = 7
batch_size = 8
####setup####

input_img_paths = sorted(
    [
        os.path.join(train_img_dir, fname)
        for fname in os.listdir(train_img_dir)
        if fname.endswith(".png")
    ]
)

target_img_paths = sorted(
    [
        os.path.join(train_mask_dir, fname)
        for fname in os.listdir(train_mask_dir)
        if fname.endswith(".png")
    ]
)
print("Number of samples:", len(input_img_paths))
img = keras.Input(shape=(384, 384, 3))

o = selfCrossPooling(nClasses, img)
o = (layers.Activation('softmax'))(o)
gp_model6 = keras.Model(img, o)
gp_model6.summary()

train_samples = len(input_img_paths)

rn.Random(2021).shuffle(input_img_paths)
rn.Random(2021).shuffle(target_img_paths)
train_input_img_paths = input_img_paths
train_target_img_paths = target_img_paths

df_train = DataFrame(train_input_img_paths,columns=['filename'])
df_train_target = DataFrame(train_target_img_paths,columns=['filename'])

data_gen_args = dict(rescale=1./255,
                     rotation_range=90,
                     width_shift_range=0.2,
                     height_shift_range=0.2,
                     zoom_range=0.2,
                     fill_mode='constant',
                     preprocessing_function=random_adjust_saturation)

data_gen_args_mask = dict(rescale=1.,
                     rotation_range=90,
                     width_shift_range=0.2,
                     height_shift_range=0.2,
                     zoom_range=0.2,
                     fill_mode='constant'
                     )

image_datagen = ImageDataGenerator(**data_gen_args)
mask_datagen = ImageDataGenerator(**data_gen_args_mask)
seed = 2021
image_generator = image_datagen.flow_from_dataframe(
    df_train,
    target_size=img_size,
    class_mode=None,
    batch_size=batch_size,
    seed=seed)

mask_generator = mask_datagen.flow_from_dataframe(
    df_train_target,
    target_size=img_size,
    color_mode='grayscale',
    class_mode=None,
    batch_size=batch_size,
    seed=seed)

def train_generator(image_generator_c, mask_generator):
    while True:
        yield (image_generator_c.next(), mask_generator.next())

adam = Adam(lr=0.001)
gp_model6.compile(optimizer=adam, loss=['sparse_categorical_crossentropy'], metrics=['sparse_categorical_accuracy'])
modelpath = "./gp_model6class" + ".h5"
callbacks = [LearningRateScheduler(scheduler)]

hist = gp_model6.fit(train_generator(image_generator, mask_generator),
                     steps_per_epoch=int(np.ceil(train_samples/batch_size)), epochs=60, verbose=0, callbacks=callbacks)
gp_model6.save(modelpath)


