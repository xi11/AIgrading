#Pyramid Scene Parsing Network  CVPR 2017
# https://github.com/divamgupta/image-segmentation-keras/blob/master/keras_segmentation/models/pspnet.py
# resize->conv->BN->relu->attention->add

import numpy as np
import tensorflow as tf
from tensorflow.keras import layers
from tensorflow.keras.layers import *
from Res50MultiAttention import poolRes50_attention

def resize_image(inp, s):
    return layers.Lambda(lambda x: tf.keras.backend.resize_images(x, height_factor=s[0], width_factor=s[1], data_format='channels_last', interpolation='bilinear'))(inp)

def resize_image_final(inp, s):
    return layers.Lambda(lambda x: tf.keras.backend.resize_images(x, height_factor=s[0], width_factor=s[1], data_format='channels_last', interpolation='nearest'))(inp)

def depth_pool(inp):
    return layers.Lambda(lambda x: tf.math.reduce_sum(x, 3, keepdims=True))(inp)

def pool_block(feats, pool_factor):
    h = tf.keras.backend.int_shape(feats)[1]
    w = tf.keras.backend.int_shape(feats)[2]

    pool_size = strides = [
        int(np.round(float(h) / pool_factor)),
        int(np.round(float(w) / pool_factor))]

    x = layers.AveragePooling2D(pool_size, strides=strides, padding='same')(feats)
    x = Conv2D(128, (1, 1), padding='same', use_bias=False, kernel_initializer='he_uniform')(x)
    x = BatchNormalization()(x)
    x = Activation('relu')(x)
    x = resize_image(x, strides)
    return x


def cross_attention_pooling(x1, x2):
    nc = tf.keras.backend.int_shape(x1)[3]
    h = tf.keras.backend.int_shape(x1)[1]
    w = tf.keras.backend.int_shape(x1)[2]

    satt = tf.math.multiply(x1, x2)
    satt = layers.Activation('sigmoid')(depth_pool(satt))

    cons = tf.constant(1.0, dtype=tf.float32, shape=(h, w, 1))
    sattSub = tf.math.subtract(cons, satt)
    return satt, sattSub


def selfCrossPsp(n_classes, img_c):
    x1, x2, o0 = poolRes50_attention(img_c)
    x3 = o0
    nc_o = tf.keras.backend.int_shape(o0)[3]

    x1 = layers.Conv2D(nc_o, (1, 1), padding='same', use_bias=False, kernel_initializer='he_uniform')(x1)
    x1 = layers.BatchNormalization()(x1)
    x1 = layers.Activation('relu')(x1)
    x2 = layers.Conv2D(nc_o, (1, 1), padding='same', use_bias=False, kernel_initializer='he_uniform')(x2)
    x2 = layers.BatchNormalization()(x2)
    x2 = layers.Activation('relu')(x2)

    pool_factor = [1, 2, 3, 6]
    pool_out1 = [x1]
    for p in pool_factor:
        pooled = pool_block(x1, p)
        pool_out1.append(pooled)
    x1 = layers.Concatenate(axis=-1)(pool_out1)

    pool_out2 = [x2]
    for p in pool_factor:
        pooled = pool_block(x2, p)
        pool_out2.append(pooled)
    x2 = layers.Concatenate(axis=-1)(pool_out2)

    pool_out3 = [x3]
    for p in pool_factor:
        pooled = pool_block(x3, p)
        pool_out3.append(pooled)
    x3 = layers.Concatenate(axis=-1)(pool_out3)

    s12, s12c = cross_attention_pooling(x1, x2)
    s13, s13c = cross_attention_pooling(x1, x3)
    s23, s23c = cross_attention_pooling(x2, x3)

    o13 = tf.math.multiply(s12c, x3)
    o13 = layers.Add()([o13, x3])
    o1 = o13

    o22 = tf.math.multiply(s13, x3)
    o22 = layers.Add()([o22, x3])
    o2 = o22

    o32 = tf.math.multiply(s23, x3)
    o32 = layers.Add()([o32, x3])
    o3 = o32

    o = layers.Concatenate()([o1, o2, o3])

    o = layers.Conv2D(256, (1, 1), use_bias=False, kernel_initializer='he_uniform')(o)
    o = layers.BatchNormalization()(o)
    o = layers.Activation('relu')(o)

    o = layers.Conv2D(32, (3, 3), padding='same', use_bias=False, kernel_initializer='he_uniform')(o) 
    o = layers.BatchNormalization()(o)
    o = layers.Activation('relu')(o)

    o = layers.Conv2D(n_classes, (1, 1), padding='same', use_bias=False, kernel_initializer='he_uniform')(o)
    o = resize_image(o, (8, 8))

    return o





