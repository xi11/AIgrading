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

#resnet50 source: https://github.com/fchollet/deep-learning-models/blob/master/resnet50.py

from tensorflow import keras
import tensorflow as tf
from tensorflow.keras.models import *
from tensorflow.keras import layers
from tensorflow.keras.layers import *

def one_side_pad(x):
    x = layers.ZeroPadding2D((1, 1))(x)
    x = layers.Lambda(lambda x: x[:, :-1, :-1, :])(x)
    return x

def resize_image(inp, s):
    return layers.Lambda(lambda x: tf.keras.backend.resize_images(x, height_factor=s[0], width_factor=s[1], data_format='channels_last', interpolation='bilinear'))(inp)

def depth_pool(inp):
    return layers.Lambda(lambda x: tf.math.reduce_mean(x, 3, keepdims=True))(inp)

def identity_block(input_tensor, kernel_size, filters, stage, block):
    """The identity block is the block that has no conv layer at shortcut.
    # Arguments
        input_tensor: input tensor
        kernel_size: defualt 3, the kernel size of middle conv layer at
                     main path
        filters: list of integers, the filterss of 3 conv layer at main path
        stage: integer, current stage label, used for generating layer names
        block: 'a','b'..., current block label, used for generating layer names
    # Returns
        Output tensor for the block.
    """
    filters1, filters2, filters3 = filters

    conv_name_base = 'res' + str(stage) + block + '_branch'
    bn_name_base = 'bn' + str(stage) + block + '_branch'

    x = layers.Conv2D(filters1, (1, 1), use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '2a')(input_tensor)
    x = layers.BatchNormalization(name=bn_name_base + '2a')(x)
    x = layers.Activation('relu')(x)

    x = layers.Conv2D(filters2, kernel_size, padding='same', use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '2b')(x)
    x = layers.BatchNormalization(name=bn_name_base + '2b')(x)
    x = layers.Activation('relu')(x)

    x = layers.Conv2D(filters3, (1, 1), use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '2c')(x)
    x = layers.BatchNormalization(name=bn_name_base + '2c')(x)

    x = layers.add([x, input_tensor])
    x = layers.Activation('relu')(x)
    return x

def conv_block(input_tensor, kernel_size, filters, stage, block, strides=(2, 2)):
    """conv_block is the block that has a conv layer at shortcut
    # Arguments
        input_tensor: input tensor
        kernel_size: defualt 3, the kernel size of middle conv layer at
                     main path
        filters: list of integers, the filterss of 3 conv layer at main path
        stage: integer, current stage label, used for generating layer names
        block: 'a','b'..., current block label, used for generating layer names
    # Returns
        Output tensor for the block.
    Note that from stage 3, the first conv layer at main path is with
    strides=(2,2) and the shortcut should have strides=(2,2) as well
    """
    filters1, filters2, filters3 = filters
    conv_name_base = 'res' + str(stage) + block + '_branch'
    bn_name_base = 'bn' + str(stage) + block + '_branch'

    x = layers.Conv2D(filters1, (1, 1), strides=strides, use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '2a')(input_tensor)
    x = layers.BatchNormalization(name=bn_name_base + '2a')(x)
    x = layers.Activation('relu')(x)


    x = layers.Conv2D(filters2, kernel_size, padding='same', use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '2b')(x)
    x = layers.BatchNormalization(name=bn_name_base + '2b')(x)
    x = layers.Activation('relu')(x)

    x = layers.Conv2D(filters3, (1, 1), use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '2c')(x)
    x = layers.BatchNormalization(name=bn_name_base + '2c')(x)

    shortcut = layers.Conv2D(filters3, (1, 1), strides=strides, use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '1')(input_tensor)
    shortcut = layers.BatchNormalization(name=bn_name_base + '1')(shortcut)

    x = layers.add([x, shortcut])
    x = layers.Activation('relu')(x)
    return x

def conv_block_atrous(input_tensor, kernel_size, filters, stage, block, strides=(1, 1)):
    """conv_block is the block that has a conv layer at shortcut
    # Arguments
        input_tensor: input tensor
        kernel_size: defualt 3, the kernel size of middle conv layer at
                     main path
        filters: list of integers, the filterss of 3 conv layer at main path
        stage: integer, current stage label, used for generating layer names
        block: 'a','b'..., current block label, used for generating layer names
    # Returns
        Output tensor for the block.
    Note that from stage 3, the first conv layer at main path is with
    strides=(2,2) and the shortcut should have strides=(2,2) as well
    """
    filters1, filters2, filters3 = filters
    conv_name_base = 'res' + str(stage) + block + '_branch'
    bn_name_base = 'bn' + str(stage) + block + '_branch'

    x = layers.Conv2D(filters1, (1, 1), strides=strides, use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '2a')(input_tensor)
    x = layers.BatchNormalization(name=bn_name_base + '2a')(x)
    x = layers.Activation('relu')(x)

    x = layers.Conv2D(filters2, kernel_size, padding='same',dilation_rate=(2, 2), use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '2b')(x)
    x = layers.BatchNormalization(name=bn_name_base + '2b')(x)
    x = layers.Activation('relu')(x)

    x = layers.Conv2D(filters3, (1, 1), use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '2c')(x)
    x = layers.BatchNormalization(name=bn_name_base + '2c')(x)

    shortcut = layers.Conv2D(filters3, (1, 1), strides=strides, use_bias=False, kernel_initializer='he_uniform', name=conv_name_base + '1')(input_tensor)
    shortcut = layers.BatchNormalization(name=bn_name_base + '1')(shortcut)

    x = layers.add([x, shortcut])
    x = layers.Activation('relu')(x)
    return x

def supres50_a(img_a):
    x = layers.AveragePooling2D((4, 4), strides=(4, 4))(img_a)
    x = layers.Conv2D(16, (7, 7), strides=(1, 1), padding='same', use_bias=False, kernel_initializer='he_uniform', name='conv1a')(x)

    x = layers.BatchNormalization(name='bn_conv1a')(x)
    x = layers.Activation('relu')(x)

    x = conv_block(x, 3, [16, 16, 64], stage=2, block='aa')
    x = identity_block(x, 3, [16, 16, 64], stage=2, block='ab')
    x = identity_block(x, 3, [16, 16, 64], stage=2, block='ac')

    x = conv_block_atrous(x, 3, [32, 32, 128], stage=3, block='aa')
    x = identity_block(x, 3, [32, 32, 128], stage=3, block='ab')
    x = identity_block(x, 3, [32, 32, 128], stage=3, block='ac')
    x = identity_block(x, 3, [32, 32, 128], stage=3, block='ad')
    Sa1 = x
    Sa2 = layers.GlobalAveragePooling2D()(x)
    Sa2 = layers.Dense(64)(Sa2)
    Sa2 = layers.Activation('relu')(Sa2)
    Sa2 = layers.Dense(128)(Sa2)
    Sa2 = layers.Activation('sigmoid')(Sa2)  # channel-wise attention

    Sa3 = depth_pool(x)
    Sa3 = layers.Conv2D(128, (7, 7), strides=(1, 1), padding='same', use_bias=False, kernel_initializer='he_uniform')(Sa3)
    Sa3 = layers.Activation('sigmoid')(Sa3) #spatial-wise attention

    return Sa1, Sa2, Sa3

def supres50_b(img_b, Sa2, Sa3):
    x = layers.AveragePooling2D((2, 2), strides=(2, 2))(img_b)
    x = layers.Conv2D(16, (7, 7), strides=(1, 1), padding='same', use_bias=False, kernel_initializer='he_uniform', name='conv1b')(x)

    x = layers.BatchNormalization(name='bn_conv1b')(x)
    x = layers.Activation('relu')(x)

    x = conv_block(x, 3, [16, 16, 64], stage=2, block='ba')
    x = identity_block(x, 3, [16, 16, 64], stage=2, block='bb')
    x = identity_block(x, 3, [16, 16, 64], stage=2, block='bc')

    x = conv_block(x, 3, [32, 32, 128], stage=3, block='ba')
    x = identity_block(x, 3, [32, 32, 128], stage=3, block='bb')
    x = identity_block(x, 3, [32, 32, 128], stage=3, block='bc')
    x = identity_block(x, 3, [32, 32, 128], stage=3, block='bd')

    f1 = layers.Multiply()([x, Sa2])
    f1 = layers.Multiply()([f1, Sa3])
    x = layers.Add()([x, f1])

    x = conv_block_atrous(x, 3, [64, 64, 256], stage=4, block='ba')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='bb')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='bc')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='bd')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='be')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='bf')
    Sb1 = x
    Sb2 = layers.GlobalAveragePooling2D()(x)
    Sb2 = layers.Dense(128)(Sb2)
    Sb2 = layers.Activation('relu')(Sb2)
    Sb2 = layers.Dense(256)(Sb2)
    Sb2 = layers.Activation('sigmoid')(Sb2)  # channel-wise attention

    Sb3 = depth_pool(x)
    Sb3 = layers.Conv2D(256, (7, 7), strides=(1, 1), padding='same', use_bias=False, kernel_initializer='he_uniform')(Sb3)
    Sb3 = layers.Activation('sigmoid')(Sb3)  # spatial-wise attention

    return Sb1, Sb2, Sb3

def supres50_c(img_c, Sb2, Sb3):
    x = layers.Conv2D(16, (7, 7), strides=(1, 1), padding='same', use_bias=False, kernel_initializer='he_uniform', name='conv1c')(img_c)
    x = layers.BatchNormalization(name='bn_conv1c')(x)
    x = layers.Activation('relu')(x)

    x = conv_block(x, 3, [16, 16, 64], stage=2, block='ca')
    x = identity_block(x, 3, [16, 16, 64], stage=2, block='cb')
    x = identity_block(x, 3, [16, 16, 64], stage=2, block='cc')

    x = conv_block(x, 3, [32, 32, 128], stage=3, block='ca')
    x = identity_block(x, 3, [32, 32, 128], stage=3, block='cb')
    x = identity_block(x, 3, [32, 32, 128], stage=3, block='cc')
    x = identity_block(x, 3, [32, 32, 128], stage=3, block='cd')

    x = conv_block(x, 3, [64, 64, 256], stage=4, block='ca')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='cb')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='cc')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='cd')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='ce')
    x = identity_block(x, 3, [64, 64, 256], stage=4, block='cf')

    f2 = layers.Multiply()([x, Sb2])
    f2 = layers.Multiply()([f2, Sb3])
    x = layers.Add()([x, f2])

    x = conv_block_atrous(x, 3, [128, 128, 512], stage=5, block='ca')
    x = identity_block(x, 3, [128, 128, 512], stage=5, block='cb')
    x = identity_block(x, 3, [128, 128, 512], stage=5, block='cc')
    Sc = x

    return Sc

def Res50_attention(img):
    Sa1, Sa2, Sa3 = supres50_a(img)
    Sb1, Sb2, Sb3 = supres50_b(img, Sa2, Sa3)
    Sc = supres50_c(img, Sb2, Sb3)
    return Sa1, Sb1,  Sc